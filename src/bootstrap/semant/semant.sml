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
structure Semant :> SEMANT = struct
   exception InternalError of string
   exception TypeError of StreamPos.pos * string * string * Types.Type * string * Types.Type


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

   (* Search a symbol list and error if a duplicate name is found. *)
   fun findDupEle lst =
      case ListMisc.findDup Symbol.nameGt lst of
         SOME dup => raise Symbol.SymbolError (dup, "List already includes a symbol with this name.")
       | _ => ()

   (* Wrap Symtab.insert, raising the appropriate exceptions. *)
   fun insertSym ts (sym, entry) =
      if Symtab.inDomain (SymtabStack.top ts) sym then
         raise Symbol.SymbolError (sym, "A symbol with this name already exists in this scope.")
      else
         Symtab.insert (SymtabStack.top ts) (sym, entry)

   (* Likewise for Moduletab.insert. *)
   fun insertModuleSym ms (sym, entry) =
      if Moduletab.inDomain (ModuletabStack.top ms) sym then
         raise Symbol.SymbolError (sym, "A symbol with this name already exists in this scope.")
      else
         Moduletab.insert (ModuletabStack.top ms) (sym, entry)

   (* Given a (symbol * expr) list like what's found in record assignment
    * expressions and exception expressions, convert the list into a
    * (symbol * type) list that can be added into the appropriate Type value.
    * We have to accept a type checking function argument here because they're
    * all defined later on, even though it'll always be checkExpr.
    *)
   fun checkNamedExprLst f ts ms lst = let
      (* First check for duplicate entries. *)
      val _ = findDupEle (map #1 lst)
   in
      (* Now convert the list. *)
      map (fn (sym, expr) => (sym, f ts ms expr)) lst
   end

   (* Wrap lookup functions so we only have to do error handling in one place. *)
   fun lookup (f, tbl, sym) =
      f tbl sym handle _ => raise Symbol.SymbolError (sym, "Referenced symbol is unknown.")

   (* Look up an identifier in the environment.  The basic algorithm is:
    *   - For naked identifiers (ones that are not part of any module), simply
    *     look them up in the symbol table stack from most local towards the
    *     global table.
    *   - For module references, first look up the initial module reference in
    *     the module table stack from most local towards the global table.  Once
    *     that has been resolved to a module, then search back down through the
    *     AST at each module table until we get to the last part of the
    *     reference.  This is an identifier, which should be looked up in the
    *     final module's symbol table.
    * This algorithm is described more in docs/typing.
    *)
   fun lookupId ts ms [] kind =
          raise InternalError "Empty identifier list passed to lookupId"
     | lookupId ts ms (id::[]) kind = SymtabStack.lookup ts (Symbol.toSymbol (id, kind))
     | lookupId ts ms (topModule::rest) kind = let
          fun doLookup (tbl as Absyn.ModuleDecl _) [] kind =
                 raise InternalError "Identifier only consists of module references."
            | doLookup (tbl as Absyn.ModuleDecl{symtab, ...}) (id::[]) kind =
                 lookup (Symtab.lookup, symtab, Symbol.toSymbol (id, kind))
            | doLookup (tbl as Absyn.ModuleDecl{moduletab, ...}) (module::rest) kind = let
                 val nextTbl = lookup (Moduletab.lookup, moduletab,
                                       Symbol.toSymbol (module, Symbol.MODULE))
              in
                 doLookup nextTbl rest kind
              end
            | doLookup _ _ _ =
                 raise InternalError "AST node besides ModuleDecl stored in moduletab."

          val moduleTbl = lookup (ModuletabStack.lookup, ms,
                                  Symbol.toSymbol (topModule, Symbol.MODULE))
       in
          doLookup moduleTbl rest kind
       end

   (* Print out a single hash table.  This can't easily go into its own module
    * because our hash tables are made from a functor which really complicates
    * the types.  foldi is the obvious function, since this needs to work on all
    * kinds of tables.
    *)
   fun tblToString foldi tbl valFn =
      foldi (fn (k, v, str) => str ^ "\t" ^ (Symbol.toString k) ^ " => " ^ (valFn v) ^ "\n") "" tbl


   (* SEMANTIC ANALYSIS FUNCTIONS *)

   fun checkProg lst = let
      (* Create the base environment. *)
      val (ts, ms) = TempEnv.createEnv ()
   in
      checkDeclLst ts ms lst
   end

   and checkExnHandler ts ms (Absyn.ExnHandler{exnKind, sym, expr, ...}) = let
      fun isExn (Entry.EXN _) = true | isExn _ = false

      (* Since exception handlers can create new bindings (for the exception
       * value itself), we first need to create a new symbol table.
       *)
      val ts' = SymtabStack.enter (ts, Symtab.mkTable (47, SymtabStack.NotFound))
   in
      case exnKind of
         (* This is not a default handler, so lookup the exception type in the
          * environment.  If it's found (which it had better be), enter the
          * exception value into the new symbol table and check the handler
          * against that environment.
          *)
         SOME id => let val entry = lookupId ts ms id Symbol.EXN_TYPE
                    in
                        (* Exceptions and types are in the same namespace. *)
                        if isExn entry then
                           ( insertSym ts' (sym, entry) ; checkExpr ts' ms expr )
                        else
                           raise Symbol.SymbolError (sym, "Symbol is not an exception type.")
                    end
         (* The default handler only gets a skeleton entry added for the
          * exception type.
          *)
       | NONE => let val _ = insertSym ts' (sym, Entry.TYPE (Types.EXN ([], Types.FINITE)))
                 in checkExpr ts' ms expr
                 end
   end

   and checkExnHandlerLst ts ms ([], SOME default) = checkExnHandler ts ms default
     | checkExnHandlerLst ts ms (handlers, NONE) = (
          case findBadEle (checkExnHandler ts ms) handlers of
             (firstTy, SOME (ty, Absyn.ExnHandler{pos, ...})) =>
                raise TypeError (pos, "Inconsistent types in exception handler list.",
                                 "previous exception handler type", firstTy,
                                 "this exception handler type", ty)
           | (firstTy, _) => firstTy
          )
     | checkExnHandlerLst ts ms (handlers, default as SOME (Absyn.ExnHandler{pos, ...})) = let
          (* All exception handlers must have the same type, and the
           * default handler must return this same type as well.  We can
           * use previous definitions of checkExnHandlerLst to do this.
           *)
          val prevTy = checkExnHandlerLst ts ms (handlers, NONE)
          val defaultTy = checkExnHandler ts ms (Option.valOf default)
       in
          if not (Types.eq (defaultTy, prevTy)) then
             raise TypeError (pos, "Default exception handler type does not match type of previous handlers.",
                              "previous exception handler type", prevTy,
                              "default exception handler type", defaultTy)
          else
             defaultTy
       end

   and checkIdRef ts ms id = Types.BOTTOM

   and checkBranch ts ms (Absyn.RegularBranch expr) = Types.BOTTOM
     | checkBranch ts ms (Absyn.UnionBranch (id, syms)) = Types.BOTTOM

   and checkExpr ts ms (Absyn.Expr{expr, exnHandler as NONE, ...}) = checkBaseExpr ts ms expr
     | checkExpr ts ms (Absyn.Expr{expr, exnHandler as SOME ({handlers, default, pos, ...}), ...}) =
       let
          val exprTy = checkBaseExpr ts ms expr
          val handlerTy = checkExnHandlerLst ts ms (handlers, default)
       in
          if not (Types.eq (exprTy, handlerTy)) then
             raise TypeError (pos,
                              "Type of exception handler does not match type of expression.",
                              "expression type", exprTy, "exception handler type", handlerTy)
          else
             exprTy
       end

   and checkBaseExpr ts ms (Absyn.BooleanExp b) = Types.BOOLEAN
     | checkBaseExpr ts ms (Absyn.BottomExp) = Types.BOTTOM
     | checkBaseExpr ts ms (Absyn.CaseExp{test, default, branches}) = Types.BOTTOM
     | checkBaseExpr ts ms (Absyn.DeclExp{decls, expr}) = let
          (* Create a new environment for the body of the decl-expr to execute
           * in, then check it against that environment.
           *)
          val ts' = SymtabStack.enter (ts, Symtab.mkTable (47, SymtabStack.NotFound))
          val _ = checkDeclLst ts' ms decls
       in
          checkExpr ts' ms expr
       end
     | checkBaseExpr ts ms (Absyn.ExnExp{id, values}) = let
          fun exprPos (Absyn.Expr{pos, ...}) = pos

          (* Exceptions and types are in the same namespace, so make sure we
           * have an exception.
           *)
          fun getExnTy (Entry.EXN ty) = ty
            | getExnTy _ = raise Symbol.IdError (id, "Symbol is not an exception type.")

          val entry = lookupId ts ms id Symbol.EXN_TYPE
          val exnTy = getExnTy entry
          val valuesTy = Types.EXN (checkNamedExprLst checkExpr ts ms values, Types.UNVISITED)
       in
          if not (Types.eq (exnTy, valuesTy)) then
             raise TypeError (exprPos (#2 (hd values)),
                              "Type of stated expression does not match list of assignments.",
                              "exception type", exnTy, "assignment list type", valuesTy)
          else
             exnTy
       end
     | checkBaseExpr ts ms (Absyn.ExprLstExp exprs) = (
          case findBadEle (checkExpr ts ms) exprs of
             (firstTy, SOME (ty, Absyn.Expr{pos, ...})) =>
                raise TypeError (pos, "Inconsistent types in expression list.",
                                 "previous expression type", firstTy,
                                 "this expression type", ty)
           | (firstTy, _) => firstTy
          )
     | checkBaseExpr ts ms (Absyn.FunCallExp{id, args, tyArgs, ...}) = Types.BOTTOM
     | checkBaseExpr ts ms (Absyn.IdExp id) = let
          val sym = Symbol.toSymbol ((hd id), Symbol.VALUE)
       in
          case SymtabStack.find ts sym of
             SOME (Entry.VALUE ty) => ty
           | SOME _ => raise Symbol.SymbolError (sym, "Referenced symbol is not a value.")
           | NONE => raise Symbol.SymbolError (sym, "Referenced symbol is unknown.")
       end
     | checkBaseExpr ts ms (Absyn.IfExp{test as Absyn.Expr{pos=testPos, ...}, then',
                                        else' as Absyn.Expr{pos=elsePos, ...}}) = let
          val testTy = checkExpr ts ms test
          val thenTy = checkExpr ts ms then'
          val elseTy = checkExpr ts ms else'
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
     | checkBaseExpr ts ms (Absyn.IntegerExp i) = Types.INTEGER
     | checkBaseExpr ts ms (Absyn.RaiseExp expr) = ( checkExpr ts ms expr ; Types.ANY Types.UNVISITED )
     | checkBaseExpr ts ms (Absyn.RecordAssnExp lst) =
          Types.RECORD (checkNamedExprLst checkExpr ts ms lst, Types.UNVISITED)
     | checkBaseExpr ts ms (Absyn.RecordRefExp{record, ele}) = Types.BOTTOM
     | checkBaseExpr ts ms (Absyn.StringExp s) = Types.STRING

   and checkTy ts ms ast = Absyn.absynToTy ast

   and checkDecl ts ms (Absyn.Absorb{module, ...}) = ()
     | checkDecl ts ms (Absyn.FunDecl{sym, absynTy=SOME absynTy, formals, tyFormals, body, ...}) = ()
     | checkDecl ts ms (Absyn.FunDecl{sym, absynTy=NONE, formals, tyFormals, body, ...}) = ()
     | checkDecl ts ms (decl as Absyn.ModuleDecl{sym, decls, symtab, moduletab, ...}) = let
          (* Add the module to the lexical parent's environment. *)
          val _ = insertSym ts (sym, Entry.MODULE)
          val _ = insertModuleSym ms (sym, decl)

          (* Push the module's environment tables onto the stacks, so now
           * everything is done relative to this module.
           *)
          val ts' = SymtabStack.enter (ts, symtab)
          val ms' = ModuletabStack.enter (ms, moduletab)
       in
          (* Check the guts of the module against the module's environment. *)
          checkDeclLst ts' ms' decls
       end
     | checkDecl ts ms (Absyn.TyDecl{sym, absynTy, tyvars, ...}) = ()
     | checkDecl ts ms (Absyn.ValDecl{sym, absynTy=SOME absynTy, init, pos}) = let
          val declaredTy = Absyn.absynToTy absynTy
          val initTy = checkExpr ts ms init
       in
          (* Since there is a type specified, check that the return type of the
           * initializing expression does match.
           *)
          if not (Types.eq (initTy, declaredTy)) then
             raise TypeError (pos, "Type of value initializer does not match declared type.",
                              "declared type", declaredTy, "initializer type", initTy)
          else
             insertSym ts (sym, Entry.VALUE initTy)
       end
     | checkDecl ts ms (Absyn.ValDecl{sym, absynTy=NONE, init, pos}) =
          insertSym ts (sym, Entry.VALUE (checkExpr ts ms init))

   and checkDeclLst ts ms decls = let
      (* Process a block of possibly mutually recursive function declarations. *)
      fun processFunDecls ts ms funcs = let
         (* Create skeleton entries for every function in the block so they can
          * recursively call each other.  A skeleton entry consists of the LHS
          * of the declaration.  These will be added to the scope of what the
          * functions are defined in so functions may call themselves.
          *)
         fun round1 ts ms [] = ()
           | round1 ts ms (Absyn.FunDecl{sym, absynTy, formals, tyFormals, body, ...}::rest) = let
                (* Create a list of formal parameters and their Types as tuples. *)
                fun buildFormalsLst [] = []
                  | buildFormalsLst (lst: (Symbol.symbol * Absyn.Ty * Absyn.pos) list) = let
                   val symLst = map #1 lst

                   (* Check for duplicate named parameters. *)
                   val _ = findDupEle symLst

                   val tyLst = map Absyn.absynToTy (map #2 lst)
                in
                   ListPair.zip (symLst, tyLst)
                end

                (* This fun decl may not have an explicit type, so just make
                 * something up for now.  This will get resolved in round2.
                 *)
                val retTy = if Option.isSome absynTy then Absyn.absynToTy (valOf absynTy)
                            else Types.BOTTOM
                val formals = buildFormalsLst formals
             in
                (* Check that a function by this name is not already defined in
                 * this scope, though it's okay to shadow the name of a function
                 * in a higher level scope.  By checking here, we don't have to
                 * check in round2.
                 *
                 * Then check the rest of the list.
                 *)
                ( insertSym ts (sym, Entry.FUNCTION{ty=retTy, tyFormals=tyFormals, formals=formals}) ; round1 ts ms rest )
             end
           | round1 ts ms _ = raise InternalError "FunDecl list contains something other than functions."
      in
         round1 ts ms funcs
      end

      (* Process a block of possibly mutually recursive type declarations. *)
      fun processTyDecls ts ms tys = let
         (* Create skeleton entries for every type in the block so they can
          * mutually refer to each other.  Skeleton entries really just contain
          * the type's name and a dummy type since we can't look at the RHS for
          * real type information.
          *)
         fun round1 ts ms [] = ()
           | round1 ts ms (Absyn.TyDecl{sym, ...}::rest) = let
                (* User-defined types are not allowed to override the types in
                 * the global environment, since that contains the base types of
                 * the language.
                 *)
                val globalTs = SymtabStack.bottom ts
             in
                if Symtab.inDomain globalTs sym then
                   raise Symbol.SymbolError (sym, "Type identifiers may not override symbols in the global scope.")
                else
                   (* Check that a type by this name is not already defined in
                    * this scope, though it's okay to shadow the name of a type
                    * in a higher level scope.  By checking here, we don't have
                    * to check in round2.
                    *
                    * Then check the rest of the list.
                    *)
                   ( insertSym ts (sym, Entry.TYPE Types.BOTTOM) ; round1 ts ms rest )
             end
           | round1 ts ms _ = raise InternalError "TyDecl list contains something other than types."
      in
         round1 ts ms tys
      end

      (* For function and type declarations, we need to handle possibly
       * recursive declarations.  Therefore, we have to build up blocks of
       * functions and blocks of types, then process those as a unit, then go
       * back for the rest.  All other declarations are straightforward.
       *)
      fun doCheck ts ms (lst as (Absyn.FunDecl _)::decls) = let
             val (funcs, rest) = ListMisc.split (fn (Absyn.FunDecl _) => true | _ => false)
                                                lst
          in
             (processFunDecls ts ms funcs) before (doCheck ts ms rest)
          end
        | doCheck ts ms (lst as (Absyn.TyDecl _)::decls) = let
             val (tys, rest) = ListMisc.split (fn (Absyn.TyDecl _) => true | _ => false)
                                              lst
          in
             (processTyDecls ts ms tys) before (doCheck ts ms rest)
          end
        | doCheck ts ms (decl::decls) = (checkDecl ts ms decl) before (doCheck ts ms decls)
        | doCheck ts ms [] = ()
   in
      doCheck ts ms decls
   end
end
