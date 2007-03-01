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
   val checkExnHandler: Absyn.ExnHandler -> unit
   val checkIdRef: Absyn.IdRef -> unit
   val checkBranch: Absyn.Branch -> unit
   val checkExpr: Absyn.Expr -> Types.Type
   val checkBaseExpr: Absyn.BaseExpr -> Types.Type
   val checkTy: Absyn.Ty -> Types.Type
   val checkDecl: Absyn.Decl -> unit
end

structure Semant :> SEMANT =
struct
   open Error

   fun checkExnHandler (Absyn.ExnHandler{exnKind, sym, expr, symtab, ty, ...}) = ()

   and checkIdRef (Absyn.Id lst) = ()

   and checkBranch (Absyn.RegularBranch expr) = ()
     | checkBranch (Absyn.UnionBranch (id, syms, symtab)) = ()

   and checkExpr (Absyn.Expr{expr, exnHandler as NONE, ...}) = checkBaseExpr expr
     | checkExpr (Absyn.Expr{expr, exnHandler as SOME ({handlers, default, ...}), ...}) = let
          fun checkHandlers (handlers, default) = Types.BOTTOM

          val exprTy = checkBaseExpr expr
          val handlerTy = checkHandlers (handlers, default)
       in
          if not (Types.tyEqual (exprTy, handlerTy)) then
             raise TypeError ("Type of exception handler does not match type of expression.",
                              "expression type", exprTy, "exception handler type", handlerTy)
          else
             exprTy
       end

   and checkBaseExpr (Absyn.BooleanExp b) = Types.BOOLEAN
     | checkBaseExpr (Absyn.BottomExp) = Types.BOTTOM
     | checkBaseExpr (Absyn.CaseExp{test, default, branches}) = Types.BOTTOM
     | checkBaseExpr (Absyn.DeclExp{decls, expr, symtab}) = Types.BOTTOM
     | checkBaseExpr (Absyn.ExnExp{id, ty, values}) = Types.BOTTOM
     | checkBaseExpr (Absyn.ExprLstExp lst) = let
          (* All expressions in the list must have the same type.  Easiest way
           * to do that is to search for the first expr that doesn't have the
           * same type as the first expr.
           *)
          val tyList = map checkExpr lst
          val badEle = List.find (fn ty => not (Types.tyEqual (ty, hd tyList))) tyList
       in
          case badEle of
             SOME e => raise TypeError ("Inconsistent types in expression list.",
                                        "previous expression type", hd tyList,
                                        "this expression type", e)
           | NONE   => hd tyList
       end
     | checkBaseExpr (Absyn.FunCallExp{id, args, tyArgs, ...}) = Types.BOTTOM
     | checkBaseExpr (Absyn.IdExp id) = Types.BOTTOM
     | checkBaseExpr (Absyn.IfExp{test, then', else'}) = let
          val testTy = checkExpr test
          val thenTy = checkExpr then'
          val elseTy = checkExpr else'
       in
          if not (Types.tyEqual (Types.BOOLEAN, testTy)) then
             raise TypeError ("if expression must return a boolean type",
                              "expected type", Types.BOOLEAN, "if expr type", testTy)
          else
             if not (Types.tyEqual (thenTy, elseTy)) then
                raise TypeError ("then and else expressions must have the same type",
                                 "then expression type", thenTy,
                                 "else expression type", elseTy)
             else
                thenTy
       end
     | checkBaseExpr (Absyn.IntegerExp i) = Types.INTEGER
     | checkBaseExpr (Absyn.RaiseExp expr) = ( checkExpr expr ; Types.ANY Types.UNVISITED )
     | checkBaseExpr (Absyn.RecordAssnExp lst) = let
          (* We're only interested in the symbols out of this AST node. *)
          val _ = case ListMisc.findDup Symbol.nameGt (map #1 lst) of
                     SOME dup => raise SymbolError ("Record definition already includes a symbol with this name.", dup)
                   | NONE => ()
       in
          (* Construct a tuple for each element of the assignment expression and
           * use that to make the return type.  This isn't very hard.
           *)
          Types.RECORD (map (fn (sym, expr) => (sym, checkExpr expr)) lst, Types.UNVISITED)
       end
     | checkBaseExpr (Absyn.RecordRefExp{record, ele}) = Types.BOTTOM
     | checkBaseExpr (Absyn.StringExp s) = Types.STRING

   and checkTy ast = Absyn.absynToTy ast

   and checkDecl (Absyn.Absorb{module, ...}) = ()
     | checkDecl (Absyn.FunDecl{sym, absynTy, formals, tyFormals, body, symtab, ...}) = ()
     | checkDecl (Absyn.ModuleDecl{sym, decls, symtab, ...}) = ()
     | checkDecl (Absyn.TyDecl{sym, ty, absynTy, tyvars, symtab, ...}) = ()
     | checkDecl (Absyn.ValDecl{sym, ty, absynTy, init, ...}) = ()
end
