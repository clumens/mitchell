(* mitchell - experimental compiler
 * Copyright (C) 2007 Chris Lumens
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)
signature SEMANT =
sig
   (* We only need to expose a single function for checking the entire AST. *)
   val checkProg: SymtabStack.stack -> Absyn.Decl list -> unit
end

structure Semant :> SEMANT =
struct
   open Error


   (* HELPER FUNCTIONS *)

   (* All elements in the list must have the same type.  The easiest way to
    * check this is to search for the first element that doesn't have the same
    * type as the first element.  However in order to get position information
    * for raising any exceptions, we need to pair the returned type up with its
    * matching element.
    *)
   fun findBadEle f lst = let
      val lst' = ListPair.zip (map f lst, lst)
      val firstTy = #1 (hd lst')
      val badEle = List.find (fn (ty, expr) => not (Types.eq (ty, firstTy))) lst'
   in
      case badEle of
         SOME x => (firstTy, SOME x)
       | NONE => (firstTy, NONE)
   end

   (* Wrap Symtab.insert, raising the appropriate exceptions. *)
   fun insertSym ts (sym, entry) =
      Symtab.insert (SymtabStack.top ts) (sym, entry)
      handle Symtab.Duplicate => raise SymbolError (sym, "A symbol with this name already exists in this scope.")


   (* SEMANTIC ANALYSIS FUNCTIONS *)

   fun checkProg ts lst =
      app (checkDecl ts) lst

   and checkExnHandler ts (Absyn.ExnHandler{exnKind, sym, expr, symtab, ...}) =
      Types.BOTTOM

   and checkExnHandlerLst ts ([], SOME default) = checkExnHandler ts default
     | checkExnHandlerLst ts (handlers, NONE) = (
          case findBadEle (checkExnHandler ts) handlers of
             (firstTy, SOME (ty, Absyn.ExnHandler{pos, ...})) =>
                raise TypeError (pos, "Inconsistent types in exception handler list.",
                                 "previous exception handler type", firstTy,
                                 "this exception handler type", ty)
           | (firstTy, _) => firstTy
          )
     | checkExnHandlerLst ts (handlers, default as SOME (Absyn.ExnHandler{pos, ...})) = let
          (* All exception handlers must have the same type, and the
           * default handler must return this same type as well.  We can
           * use previous definitions of checkExnHandlerLst to do this.
           *)
          val prevTy = checkExnHandlerLst ts (handlers, NONE)
          val defaultTy = checkExnHandler ts (Option.valOf default)
       in
          if not (Types.eq (defaultTy, prevTy)) then
             raise TypeError (pos, "Default exception handler type does not match type of previous handlers.",
                              "previous exception handler type", prevTy,
                              "default exception handler type", defaultTy)
          else
             defaultTy
       end

   and checkIdRef ts id = ()

   and checkBranch ts (Absyn.RegularBranch expr) = ()
     | checkBranch ts (Absyn.UnionBranch (id, syms, symtab)) = ()

   and checkExpr ts (Absyn.Expr{expr, exnHandler as NONE, ...}) = checkBaseExpr ts expr
     | checkExpr ts (Absyn.Expr{expr, exnHandler as SOME ({handlers, default, pos, ...}), ...}) =
       let
          val exprTy = checkBaseExpr ts expr
          val handlerTy = checkExnHandlerLst ts (handlers, default)
       in
          if not (Types.eq (exprTy, handlerTy)) then
             raise TypeError (pos,
                              "Type of exception handler does not match type of expression.",
                              "expression type", exprTy, "exception handler type", handlerTy)
          else
             exprTy
       end

   and checkBaseExpr ts (Absyn.BooleanExp b) = Types.BOOLEAN
     | checkBaseExpr ts (Absyn.BottomExp) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.CaseExp{test, default, branches}) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.DeclExp{decls, expr, symtab}) = let
          (* Create a new environment for the body of the decl-expr to execute
           * in, then check it against that environment.
           *)
          val ts' = SymtabStack.enter (ts, symtab)
          val _ = checkDeclLst ts' decls
       in
          checkExpr ts' expr
       end
     | checkBaseExpr ts (Absyn.ExnExp{id, values}) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.ExprLstExp exprs) = (
          case findBadEle (checkExpr ts) exprs of
             (firstTy, SOME (ty, Absyn.Expr{pos, ...})) =>
                raise TypeError (pos, "Inconsistent types in expression list.",
                                 "previous expression type", firstTy,
                                 "this expression type", ty)
           | (firstTy, _) => firstTy
          )
     | checkBaseExpr ts (Absyn.FunCallExp{id, args, tyArgs, ...}) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.IdExp id) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.IfExp{test as Absyn.Expr{pos=testPos, ...}, then',
                                     else' as Absyn.Expr{pos=elsePos, ...}}) = let
          val testTy = checkExpr ts test
          val thenTy = checkExpr ts then'
          val elseTy = checkExpr ts else'
       in
          if not (Types.eq (Types.BOOLEAN, testTy)) then
             raise TypeError (testPos, "if expression must return a boolean type",
                              "expected type", Types.BOOLEAN, "if expr type", testTy)
          else
             if not (Types.eq (thenTy, elseTy)) then
                raise TypeError (elsePos, "then and else expressions must have the same type",
                                 "then expression type", thenTy,
                                 "else expression type", elseTy)
             else
                thenTy
       end
     | checkBaseExpr ts (Absyn.IntegerExp i) = Types.INTEGER
     | checkBaseExpr ts (Absyn.RaiseExp expr) = ( checkExpr ts expr ; Types.ANY Types.UNVISITED )
     | checkBaseExpr ts (Absyn.RecordAssnExp lst) = let
          (* We're only interested in the symbols out of this AST node. *)
          val _ = case ListMisc.findDup Symbol.nameGt (map #1 lst) of
                     SOME dup => raise SymbolError (dup, "Record definition already includes a symbol with this name.")
                   | NONE => ()
       in
          (* Construct a tuple for each element of the assignment expression and
           * use that to make the return type.  This isn't very hard.
           *)
          Types.RECORD (map (fn (sym, expr) => (sym, checkExpr ts expr)) lst, Types.UNVISITED)
       end
     | checkBaseExpr ts (Absyn.RecordRefExp{record, ele}) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.StringExp s) = Types.STRING

   and checkTy ts ast = Absyn.absynToTy ast

   and checkDecl ts (Absyn.Absorb{module, ...}) = ()
     | checkDecl ts (Absyn.FunDecl{sym, absynTy=SOME absynTy, formals, tyFormals, body,
                                   symtab, ...}) = ()
     | checkDecl ts (Absyn.FunDecl{sym, absynTy=NONE, formals, tyFormals, body, symtab,
                                   ...}) = ()
     | checkDecl ts (Absyn.ModuleDecl{sym, decls, symtab, ...}) = let
          (* Add the module to the lexical parent's table. *)
          val _ = insertSym ts (sym, Symtab.SYM_MODULE symtab)
       in
          (* Check the guts of the module against the module's new environment. *)
          checkDeclLst (SymtabStack.enter (ts, symtab)) decls
       end
     | checkDecl ts (Absyn.TyDecl{sym, absynTy, tyvars, symtab, ...}) = ()
     | checkDecl ts (Absyn.ValDecl{sym, absynTy=SOME absynTy, init, pos}) = let
          val declaredTy = Absyn.absynToTy absynTy
          val initTy = checkExpr ts init
       in
          (* Since there is a type specified, check that the return type of the
           * initializing expression does match.
           *)
          if not (Types.eq (initTy, declaredTy)) then
             raise TypeError (pos, "Type of value initializer does not match declared type.",
                              "declared type", declaredTy, "initializer type", initTy)
          else
             insertSym ts (sym, Symtab.SYM_VALUE initTy)
       end
     | checkDecl ts (Absyn.ValDecl{sym, absynTy=NONE, init, pos}) =
          insertSym ts (sym, Symtab.SYM_VALUE (checkExpr ts init))

   and checkDeclLst ts decls = let
      (* Process a block of possibly mutually recursive function declarations. *)
      fun processFunDecls ts funcs = ()

      (* Process a block of possibly mutually recursive type declarations. *)
      fun processTyDecls ts tys = ()

      (* For function and type declarations, we need to handle possibly
       * recursive declarations.  Therefore, we have to build up blocks of
       * functions and blocks of types , then process those as a unit, then go
       * back for the rest.  All other declarations are straightforward.
       *)
      fun doCheck ts (lst as (Absyn.FunDecl _)::decls) = let
             val (funcs, rest) = ListMisc.split (fn (Absyn.FunDecl _) => true | _ => false)
                                                lst
          in
             (processFunDecls ts funcs) before (doCheck ts rest)
          end
        | doCheck ts (lst as (Absyn.TyDecl _)::decls) = let
             val (tys, rest) = ListMisc.split (fn (Absyn.TyDecl _) => true | _ => false)
                                              lst
          in
             (processTyDecls ts tys) before (doCheck ts rest)
          end
        | doCheck ts (decl::decls) = (checkDecl ts decl) before (doCheck ts decls)
        | doCheck ts [] = ()
   in
      doCheck ts decls
   end
end
