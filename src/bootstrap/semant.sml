(* mitchell - experimental compiler
 * Copyright (C) 2006, 2007 Chris Lumens
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


   (* HELPER FUNCTIONS *)

   (* Given a list and a comparison function (for sorting), return the first
    * duplicate item.  The comparison function must return a boolean (not an
    * order, sadly).  The function returns an option with the duplicate item.
    *)
   fun findDup cmp lst = let
      fun areEq (ele::next::_) = cmp (ele, next)
        | areEq _ = false

      fun loop (ele::lst) = if areEq (ele::lst) then SOME(ele)
                            else loop lst
        | loop _ = NONE

      (* Sorting the list first makes everything else work. *)
      val lst' = ListMergeSort.sort cmp lst
   in
      loop lst'
   end

   (* findDup is nicely generic, but in this file we'll probably only ever need
    * to do the following:
    *)
   val symCmpFunc = fn (a: Symbol.symbol, b:Symbol.symbol) =>
                       BaseTy.mstringToString (#1 a) > BaseTy.mstringToString (#1 b)


   (* SEMANTIC ANALYSIS FUNCTIONS *)

   fun checkExnHandler (ExnHandler{exnKind, sym, expr, symtab, ty, ...}) = ()

   and checkIdRef (Id lst) = ()

   and checkBranch (RegularBranch expr) = ()
     | checkBranch (UnionBranch (id, syms, symtab)) = ()

   and checkExpr (Expr{expr, ty, exnHandler, ...}) = checkBaseExpr expr

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
          val _ = case findDup symCmpFunc (map #1 lst) of
                     SOME dup => raise SymbolError ("A symbol with this name already exists in this record type.", dup)
                   | NONE => ()
       in
          (* Construct a tuple for each element of the assignment expression and
           * use that to make the return type.  This isn't very hard.
           *)
          Types.RECORD (map (fn (sym, expr) => (sym, checkExpr expr)) lst, Types.UNVISITED)
       end
     | checkBaseExpr (RecordRefExp{record, ele}) = Types.BOTTOM
     | checkBaseExpr (StringExp s) = Types.STRING

   and checkTy (BottomTy _) = ()
     | checkTy (ExnTy{exn', ...}) = ()
     | checkTy (IdTy{id, ...}) = ()
     | checkTy (ListTy{lst, ...}) = ()
     | checkTy (RecordTy{record, ...}) = ()
     | checkTy (UnionTy{tycons, ...}) = ()

   and checkDecl (Absorb{module, ...}) = ()
     | checkDecl (FunDecl{sym, absynTy, formals, tyFormals, body, symtab, ...}) = ()
     | checkDecl (ModuleDecl{sym, decls, symtab, ...}) = ()
     | checkDecl (TyDecl{sym, ty, absynTy, tyvars, symtab, ...}) = ()
     | checkDecl (ValDecl{sym, ty, absynTy, init, ...}) = ()
end
