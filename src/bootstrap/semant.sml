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
   (* Type checking functions. *)
   val checkExnHandler: SymtabStack.stack -> Absyn.ExnHandler -> Types.Type
   val checkIdRef: SymtabStack.stack -> Absyn.IdRef -> unit
   val checkBranch: SymtabStack.stack -> Absyn.Branch -> unit
   val checkExpr: SymtabStack.stack -> Absyn.Expr -> Types.Type
   val checkBaseExpr: SymtabStack.stack -> Absyn.BaseExpr -> Types.Type
   val checkTy: SymtabStack.stack -> Absyn.Ty -> Types.Type
   val checkDecl: SymtabStack.stack -> Absyn.Decl -> unit
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


   (* SEMANTIC ANALYSIS FUNCTIONS *)

   fun checkExnHandler ts (Absyn.ExnHandler{exnKind, sym, expr, symtab, ty, ...}) =
      Types.BOTTOM

   and checkIdRef ts (Absyn.Id lst) = ()

   and checkBranch ts (Absyn.RegularBranch expr) = ()
     | checkBranch ts (Absyn.UnionBranch (id, syms, symtab)) = ()

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
     | checkBaseExpr ts (Absyn.DeclExp{decls, expr, symtab}) = Types.BOTTOM
     | checkBaseExpr ts (Absyn.ExnExp{id, ty, values}) = Types.BOTTOM
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
                     SOME dup => raise SymbolError ("Record definition already includes a symbol with this name.", dup)
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
     | checkDecl ts (Absyn.FunDecl{sym, absynTy, formals, tyFormals, body, symtab, ...}) = ()
     | checkDecl ts (Absyn.ModuleDecl{sym, decls, symtab, ...}) = ()
     | checkDecl ts (Absyn.TyDecl{sym, ty, absynTy, tyvars, symtab, ...}) = ()
     | checkDecl ts (Absyn.ValDecl{sym, ty, absynTy, init, ...}) = ()
end
