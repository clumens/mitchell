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

(* This structure defines the format of the abstact syntax tree. *)
structure Absyn = struct
   type pos = StreamPos.pos

   (* A reference to a (hopefully) existing identifier.  This is something we
    * will look up in the symbol table later on, not something to be stored in
    * the symbol table.
    *)
   type id = MString.mstring list

   (* Exception handler expression.  exnKind is optional in the default case,
    * where we handle all types of exceptions and therefore aren't given
    * an exception type.
    *)
   datatype ExnHandler = ExnHandler of {exnKind: id option, sym: Symbol.symbol, expr: Expr,
                                        pos: pos}

   (* Allow type constructors to appear as the branch of a case expression,
    * with value bindings for the elements in the constructor.
    *)
   and Branch = RegularBranch of BaseExpr
              | UnionBranch of id * Symbol.symbol list

   (* Wrap the basic expression type in things every expression has -
    * a position, a type, and a possible exception handler.
    *)
   and Expr = Expr of {expr: BaseExpr, pos: pos,
                       exnHandler: {handlers: ExnHandler list, default: ExnHandler option,
                                    pos: pos} option}

   and BaseExpr = BooleanExp of bool
                | BottomExp
                | CaseExp of {test: Expr, default: Expr option,
                              branches: (Branch * Expr) list}
                | DeclExp of {decls: Decl list, expr: Expr}
                | ExnExp of {id: id, values: (Symbol.symbol * Expr) list}
                | ExprLstExp of Expr list
                | FunCallExp of {id: id, args: Expr list, tyArgs: Ty list,
                                 frees: Symbol.symbol list}
                | IdExp of id
                | IfExp of {test: Expr, then': Expr, else': Expr}
                | IntegerExp of int
                | RaiseExp of Expr
                | RecordAssnExp of (Symbol.symbol * Expr) list
                | RecordRefExp of {record: BaseExpr, ele: Symbol.symbol list}
                | StringExp of MString.mstring

   and Ty = BottomTy of pos
          | ExnTy of {exn': (Symbol.symbol * Ty * pos) list, pos: pos}
          | IdTy of {id: id, pos: pos}
          | ListTy of {lst: Ty, pos: pos}
          | RecordTy of {record: (Symbol.symbol * Ty * pos) list, pos: pos}
          | UnionTy of {tycons: (Symbol.symbol * Ty option * pos) list, pos: pos}

   and Decl = Absorb of {module: id, pos: pos}
              (* Each element of calls must be a FunCallExp. *)
            | FunDecl of {sym: Symbol.symbol, absynTy: Ty option, pos: pos,
                          formals: (Symbol.symbol * Ty * pos) list,
                          tyFormals: Symbol.symbol list, calls: Expr list, body: Expr}
            | ModuleDecl of {sym: Symbol.symbol, decls: Decl list,
                             moduletab: Decl Moduletab.hash_table,
                             symtab: Entry.Entry Symtab.hash_table, pos: pos}
            | TyDecl of {sym: Symbol.symbol, absynTy: Ty, tyvars: Symbol.symbol list option,
                         pos: pos}
            | ValDecl of {sym: Symbol.symbol, absynTy: Ty option, init: Expr, pos: pos}

   (* Convert an AST subtree into a Types.Type.  This may perhaps belong in
    * types.sml but that will create circular references between the two files.
    * This is just as good a place.
    *)
   local
      fun checkForDupes lst msg =
         case ListMisc.findDup Symbol.nameGt lst of
            SOME dup => raise Symbol.SymbolError (dup, msg)
          | NONE => ()
   in
      fun absynToTy (BottomTy _) = Types.BOTTOM
        | absynToTy (ExnTy{exn', ...}) = let
             val _ = checkForDupes (map #1 exn')"Exception definition already includes a symbol with this name."
          in
             Types.EXN (map (fn ele => (#1 ele, absynToTy (#2 ele))) exn', Types.UNVISITED)
          end
        | absynToTy (IdTy{id, ...}) =
             (* FIXME - write this one *)
             Types.BOTTOM
        | absynToTy (ListTy{lst, ...}) = Types.LIST (absynToTy lst, Types.UNVISITED)
        | absynToTy (RecordTy{record, ...}) = let
             val _ = checkForDupes (map #1 record) "Record definition already includes a symbol with this name."
          in
             Types.RECORD (map (fn ele => (#1 ele, absynToTy (#2 ele))) record, Types.UNVISITED)
          end
        | absynToTy (UnionTy{tycons, ...}) = let
             val _ = checkForDupes (map #1 tycons) "Union type definition already includdes a symbol with this name."
          in
             Types.UNION (map (fn ele => (#1 ele, Option.map absynToTy (#2 ele))) tycons,
                          Types.UNVISITED)
          end
   end

   (* Given a TextIO.outstream * string * Decl list (where a Decl list is
    * really an abstract syntax tree), print the AST.  This could be
    * considered a pretty printer, if the output was actually pretty.
    *)
   fun write stream hdr ast = let
      fun say str = TextIO.output (stream, str)
      fun sayln str = TextIO.output (stream, str ^ "\n")

      fun indent 0 = ()
        | indent i = ( say "   " ; indent (i-1) )

      (* Surround the output of the function f with braces, printing a header
       * the start as well.  f should be a curried function whose last param is
       * the indentation level.
       *)
      fun << i hdr f = ( indent i ; sayln (hdr ^ " = { ") ; f (i+1) ; indent i ; sayln "}" )

      (* Print a list by calling the function f over each element of lst.  f
       * should be a curried function whose last parameter is the indentation
       * level.  In the typical use case, printLst will be passed into << as
       * its function, and << will pass the indentation level.
       *)
      fun printLst lst f i = app (f i) lst


      (* AST PRINTING HELPER FUNCTIONS *)

      fun writeTypedId i (sym, ty, _) =
         ( writeSymbol i sym ; writeTy (i+1) ty )

      and writeOptTypedId i (sym, ty, _) = 
         (writeSymbol i sym ; Option.app (writeTy (i+1)) ty)

      and writeOneBranch i (branch, expr) =
         (indent i ; sayln "branch = " ; writeBranch (i+1) branch ;
          indent i ; sayln "expr = " ; writeExpr (i+1) expr)

      and writeAssnExpr i (symbol, expr) =
         ( writeSymbol i symbol ; indent i ; sayln "expr = " ; writeExpr (i+1) expr )

      and writeSymbol i sym = ( indent i ; sayln ("sym = " ^ Symbol.toString sym) )

      and writeIdRef i id =
         ( indent i ; sayln ("id = " ^ String.concatWith "." (map MString.toString id)) )


      (* AST PRINTING FUNCTIONS *)

      and writeExnHandler i (ExnHandler{exnKind, sym, expr, ...}) =
         (indent i ; sayln "exn_handler = {" ;
          Option.app (fn v => writeIdRef (i+1) v) exnKind ;
          writeSymbol (i+1) sym ;
          indent (i+1) ; sayln "expr =" ; writeExpr (i+2) expr ;
          indent i ; sayln "}")

      and writeBranch i (RegularBranch expr) = writeBaseExpr i expr
        | writeBranch i (UnionBranch (id, lst)) =
             (indent i ; sayln "union_branch = {" ;
              writeIdRef (i+1) id ;
              << (i+1) "bindings" (printLst lst writeSymbol) ;
              indent i ; sayln "}")

      and writeExpr i (Expr{expr, exnHandler, ...}) = let
         fun writeExnHandlers i {handlers, default, pos} = let
            fun optHandler i (SOME h) = << i "default" (fn i => writeExnHandler i h)
              | optHandler i _        = ()
         in
            << i "exn_handler" (fn i => (<< (i+1) "handlers"
                                            (printLst handlers writeExnHandler) ;
                                         optHandler (i+1) default))
         end
      in
         indent i ; sayln "expr = " ; writeBaseExpr (i+1) expr ;
         Option.app (fn v => writeExnHandlers (i+1) v) exnHandler
      end

      and writeBaseExpr i (BooleanExp v) =
             (indent i ; sayln ("BOOLEAN(" ^ Bool.toString v ^ ")"))
        | writeBaseExpr i BottomExp =
             (indent i ; sayln "BOTTOM")
        | writeBaseExpr i (CaseExp{test, default, branches}) =
             (indent i ; sayln "case = {" ;
              indent (i+1) ; sayln "test =" ; writeExpr (i+2) test ;
              << (i+1) "branches" (printLst branches writeOneBranch) ;
              Option.app (fn v => (indent (i+1) ; say "default =" ; writeExpr (i+2) v)) default ;
              indent i ; sayln "}")
        | writeBaseExpr i (DeclExp{decls, expr, ...}) =
             (indent i ; say "decl = {" ;
              << (i+1) "decls" (printLst decls writeDecl) ;
              indent (i+1) ; say "expr =" ; writeExpr (i+2) expr ;
              indent i ; say "}")
        | writeBaseExpr i (ExnExp{id, values, ...}) =
             (indent i ; say "exn_expr = {" ;
              writeIdRef (i+1) id ;
              << (i+1) "values" (printLst values writeAssnExpr) ;
              indent i ; sayln "}")
        | writeBaseExpr i (ExprLstExp lst) =
             << i "expr_lst" (printLst lst writeExpr)
        | writeBaseExpr i (FunCallExp{id, args, tyArgs, ...}) =
             (indent i ; sayln "function = {" ;
              writeIdRef (i+1) id ;
              if length tyArgs > 0 then << (i+1) "tyArgs" (printLst tyArgs writeTy) else () ;
              if length args > 0 then << (i+1) "args" (printLst args writeExpr) else () ;
              indent i ; sayln "}")
        | writeBaseExpr i (IdExp v) =
             writeIdRef i v
        | writeBaseExpr i (IfExp{test, then', else'}) =
             (indent i ; sayln "if = {" ;
              indent (i+1) ; sayln "test = " ; writeExpr (i+2) test ;
              indent (i+1) ; sayln "then = " ; writeExpr (i+2) then' ;
              indent (i+1) ; sayln "else = " ; writeExpr (i+2) else' ;
              indent i ; sayln "}")
        | writeBaseExpr i (IntegerExp v) =
             (indent i ; sayln ("INTEGER(" ^ Int.toString v ^ ")"))
        | writeBaseExpr i (RaiseExp expr) =
             (indent i ; sayln "raise_expr = "; writeExpr (i+1) expr)
        | writeBaseExpr i (RecordAssnExp lst) =
             << i "record_assn_expr" (printLst lst writeAssnExpr)
        | writeBaseExpr i (RecordRefExp{record, ele}) =
             (indent i ; sayln "record_expr = {" ;
              indent (i+1) ; sayln "record =" ; writeBaseExpr (i+2) record ;
              << i "element =" ; (printLst ele writeSymbol) ;
              indent i ; sayln "}")
        | writeBaseExpr i (StringExp v) =
             (indent i ; sayln ("STRING(" ^ MString.toString v ^ ")"))

      and writeTy i (BottomTy _) = (indent i ; sayln "ty = BOTTOM")
        | writeTy i (ExnTy{exn', ...}) = << i "ty = EXN" (printLst exn' writeTypedId)
        | writeTy i (IdTy{id, ...}) = (indent i ; sayln "ty = " ; writeIdRef (i+1) id)
        | writeTy i (ListTy{lst, ...}) = (indent i ; sayln "ty = LIST " ; writeTy (i+1) lst)
        | writeTy i (RecordTy{record, ...}) = << i "ty = RECORD " (printLst record writeTypedId)
        | writeTy i (UnionTy{tycons, ...}) = << i "ty = UNION" (printLst tycons writeOptTypedId)

      and writeDecl i (Absorb{module, ...}) =
             (indent i ; sayln "absorb = " ; writeIdRef (i+1) module)
        | writeDecl i (FunDecl{sym, absynTy, formals, tyFormals, body, ...}) =
             (indent i ; sayln "fun_decl = {" ;
              writeSymbol (i+1) sym ;
              Option.app (fn v => (indent (i+1) ; sayln "retval = " ; writeTy (i+2) v)) absynTy ;
              if length tyFormals > 0 then << (i+1) "tyFormals" (printLst tyFormals writeSymbol)  else () ;
              if length formals > 0 then << (i+1) "formals" (printLst formals writeTypedId) else () ;
              indent (i+1) ; sayln "body = " ; writeExpr (i+2) body ;
              indent i ; sayln "}")
        | writeDecl i (ModuleDecl{sym, decls, ...}) =
             (indent i ; sayln "module_decl = {" ;
              writeSymbol (i+1) sym ;
              << (i+1) "decls" (printLst decls writeDecl) ;
              indent i ; sayln "}")
        | writeDecl i (TyDecl{sym, absynTy, ...}) =
             (indent i ; sayln "ty_decl = {" ;
              writeSymbol (i+1) sym ;
              writeTy (i+1) absynTy ;
              indent i ; sayln "}")
        | writeDecl i (ValDecl{sym, absynTy, init, ...}) =
             (indent i ; sayln "val_decl = {" ;
              writeSymbol (i+1) sym ;
              Option.app (fn v => (indent (i+1) ; sayln "ty = " ; writeTy (i+2) v)) absynTy ;
              indent (i+1) ; sayln "init =" ; writeExpr (i+2) init ;
              indent i ; sayln "}")
   in
      sayln ("\n" ^ hdr ^ "\n========================================") ; app (writeDecl 0) ast
   end
end
