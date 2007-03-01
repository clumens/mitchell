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

structure Semant =
struct
   open Absyn
   open Error

   fun checkExnHandler (ExnHandler{exnKind, sym, expr, symtab, ty, ...}) = ()

   and checkIdRef (Id lst) = ()

   and checkBranch (RegularBranch expr) = ()
     | checkBranch (UnionBranch (id, syms, symtab)) = ()

   and checkExpr (Expr{expr, exnHandler as NONE, ...}) = checkBaseExpr expr
     | checkExpr (Expr{expr, exnHandler as SOME ({handlers, default, ...}), ...}) = let
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

   and checkBaseExpr (BooleanExp b) = Types.BOOLEAN
     | checkBaseExpr (BottomExp) = Types.BOTTOM
     | checkBaseExpr (CaseExp{test, default, branches}) = Types.BOTTOM
     | checkBaseExpr (DeclExp{decls, expr, symtab}) = Types.BOTTOM
     | checkBaseExpr (ExnExp{id, ty, values}) = Types.BOTTOM
     | checkBaseExpr (ExprLstExp lst) = let
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
     | checkBaseExpr (FunCallExp{id, args, tyArgs, ...}) = Types.BOTTOM
     | checkBaseExpr (IdExp id) = Types.BOTTOM
     | checkBaseExpr (IfExp{test, then', else'}) = let
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
     | checkBaseExpr (IntegerExp i) = Types.INTEGER
     | checkBaseExpr (RaiseExp expr) = ( checkExpr expr ; Types.ANY Types.UNVISITED )
     | checkBaseExpr (RecordAssnExp lst) = let
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
     | checkBaseExpr (RecordRefExp{record, ele}) = Types.BOTTOM
     | checkBaseExpr (StringExp s) = Types.STRING

   and checkTy ast = absynToTy ast

   and checkDecl (Absorb{module, ...}) = ()
     | checkDecl (FunDecl{sym, absynTy, formals, tyFormals, body, symtab, ...}) = ()
     | checkDecl (ModuleDecl{sym, decls, symtab, ...}) = ()
     | checkDecl (TyDecl{sym, ty, absynTy, tyvars, symtab, ...}) = ()
     | checkDecl (ValDecl{sym, ty, absynTy, init, ...}) = ()
end
