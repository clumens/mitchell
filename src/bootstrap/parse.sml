signature PARSE =
sig
   val parse: Decode.DecFile -> Absyn.Decl list
end

structure Parse :> PARSE =
struct
   open Error
   open Tokens

   (* Read the next token from the token stream, returning the token and the
    * updated file object.
    *)
   fun eat file = let
      val (tok', file') = nextToken file
   in
      print (Tokens.toString tok' ^ "\n") ; (tok', file')
   end


   (* ERROR HANDLING FUNCTIONS *)

   (* Format and return a ParseError exception that can later be raised. *)
   fun err (tok, accepted) = let
      fun asToken kind = (0, 0, kind)

      val acceptedStr = String.concatWith " " (map Tokens.toString
                                                   (map asToken accepted))
   in
      ParseError ("FIXME", #1 tok, #2 tok,
                  "Expected token from set { " ^ acceptedStr ^ " } but got { " ^
                  Tokens.toString tok ^ " } instead.")
   end


   (* TOKEN MANIPULATION FUNCTIONS *)

   (* Given a token kind, return the underlying symbol if it's an Identifier. *)
   fun stripId (Identifier(id)) = id
     | stripId _ = raise InternalError "stripId given something besides an Identifier"

   (* Test to see if two token kinds match. *)
   fun == (Boolean(_), Boolean(_)) = true
     | == (Identifier(_), Identifier(_)) = true
     | == (Integer(_), Integer(_)) = true
     | == (String(_), String(_)) = true
     | == (kindA, kindB) = kindA = kindB

   infix ==

   (* See if a token kind is in a given set of kinds. *)
   fun inSet kind set =
      List.exists (fn ele => ele == kind) set

   (* Function for handling throw-away tokens that don't have any more
    * serious meaning to us other than that they are required by the
    * grammar.  Returns token * Decode.DecFile.
    *)
   fun checkTok (tok, file) lst = let
      fun doCheck (tok, file) expected =
         if #3 tok == expected then eat file
         else raise err (tok, [expected])

      fun loop (tok, file) (expected::[]) = doCheck (tok, file) expected
        | loop (tok, file) (expected::lst') = loop (doCheck (tok, file) expected) lst'
        | loop (tok, file) [] = raise InternalError "checkTok called with empty list"
   in
      loop (tok, file) lst
   end

   (* Extract position information from a token. *)
   fun tokenPos (tok:tokens) = (#1 tok, #2 tok)


   (* PARSING HELPER FUNCTIONS *)

   fun wrappedLst (tok, file) (openKind, closeKind) f = let
      val (tok', file') = checkTok (tok, file) openKind
      val (tok', file', ast) = f (tok', file')
      val (tok', file') = checkTok (tok', file') closeKind
   in
      (tok', file', ast)
   end


   (* PARSING FUNCTIONS *)

   (* Structure entry point - start parsing the token stream, returning an
    * abstract syntax tree.  We go through a couple hoops here because of
    * wanting to treat the start symbol correctly.
    *)
   fun parse file = let
      (* Prime the process by reading the first token from the input. *)
      val (tok, file') = nextToken file
   in
      #3 (parseStart (tok, file'))
   end

   (* start = module-decl+ *)
   and parseStart (tok, file) = let
      fun doParseStart (tok as (_, _, kind), file, lst) =
         case kind of
            Module    => let val (tok', file', ast) = parseModuleDecl (tok, file)
                         in doParseStart (tok', file', ast::lst)
                         end
          | EndOfFile => (tok, file, rev lst)
          | _         => raise err (tok, [Module, EndOfFile])
   in
      doParseStart (tok, file, [])
   end

   (* decl = absorb-symbol id | fun-decl | ty-decl | val-decl *)
   and parseDecl (tok as (_, _, kind), file) = let
      fun parseAbsorbDecl (tok, file) = let
         val (idTok, file') = checkTok (tok, file) [Absorb]
         val (tok', file', sym) = parseId (idTok, file')
      in
         (tok', file', Absyn.Absorb{module=sym, pos=tokenPos tok})
      end

      (* fun-decl = function-symbol identifier-symbol ty-formals-lst formals-lst (colon-symbol ty)? assign-symbol expr *)
      (* formals-lst = lparen-symbol typed-name-lst? rparen-symbol *)
      (* ty-formals-lst = lparen-symbol name-lst? rparen-symbol *)
      and parseFunDecl (tok, file) = let
         val (idTok, file') = checkTok (tok, file) [Function]
         val (tyFormalsTok, file', id) = parseIdentifierSym (idTok, file')
         val (formalsTok, file', tyFormals) = wrappedLst (tyFormalsTok, file') ([LParen], [RParen]) parseNameLst
         val (tok', file', formals) = wrappedLst (formalsTok, file') ([LParen], [RParen]) parseTypedNameLst
         val (tok', file', ty) = parseOptionalType (tok', file')
         val (exprTok, file') = checkTok (tok', file') [Assign]
         val (tok', file', expr) = parseExpr (exprTok, file')

         val sym = Symbol.toSymbol (id, Symbol.FUNCTION)
      in
         (tok', file', Absyn.FunDecl{sym=sym, retval=ty, pos=tokenPos tok, formals=formals,
                                     tyFormals=tyFormals, calls=[], symtab=Symbol.empty(),
                                     body=expr})
      end

      (* ty-decl = type-symbol identifier-symbol assign-symbol ty *)
      and parseTyDecl (tok, file) = let
         val (idTok, file') = checkTok (tok, file) [Type]
         val (tok', file', id) = parseIdentifierSym (idTok, file')
         val (tyTok, file') = checkTok (tok', file') [Assign]
         val (tok', file', ty) = parseTy (tyTok, file')

         val sym = Symbol.toSymbol (id, Symbol.TYPE)
      in
         (tok', file', Absyn.TyDecl{sym=sym, ty=Types.NONE_YET, absynTy=ty, pos=tokenPos tok})
      end

      (* val-decl = val-symbol identifier-symbol (colon-symbol ty)? assign-symbol expr *)
      and parseValDecl (tok, file) = let
         val (idTok, file') = checkTok (tok, file) [Val]
         val (tok', file', id) = parseIdentifierSym (idTok, file')
         val (tok', file', ty) = parseOptionalType (tok', file')
         val (exprTok, file') = checkTok (tok, file) [Assign]
         val (tok', file', expr) = parseExpr (exprTok, file')

         val sym = Symbol.toSymbol (id, Symbol.VALUE)
      in
         (tok', file', Absyn.ValDecl{sym=sym, ty=Types.NONE_YET, absynTy=ty, pos=tokenPos tok,
                                     init=expr})
      end
   in
      case kind of
         Absorb   => parseAbsorbDecl (tok, file)
       | Function => parseFunDecl (tok, file)
       | Type     => parseTyDecl (tok, file)
       | Val      => parseValDecl (tok, file)
       | _        => raise err (tok, [Absorb, Function, Type, Val])
   end

   (* exn-lst = id identifier-symbol mapsto-symbol expr (comma-symbol exn-lst)?
    *         | else-symbol identifier-symbol mapsto-symbol expr
    *)
   and parseExnLst (tok, file) = let
      fun doParseExnLst (tok as (_, _, kind), file, lst) = let
         fun handleElseBranch (tok, file, lst) = let
            val (tok', file') = checkTok (tok, file) [Else]
            val (tok', file', id) = parseIdentifierSym (tok', file')
            val (tok', file') = checkTok (tok', file') [Mapsto]
            val (tok', file', expr) = parseExpr (tok', file')

            val handler = Absyn.ExnHandler {sym=NONE, id=id, expr=expr, symtab=Symbol.empty(),
                                            ty=Types.NONE_YET, pos=tokenPos tok}
         in
            (tok', file', SOME {handlers=rev lst, default=SOME handler, ty=Types.NONE_YET,
                                pos=tokenPos tok})
         end
      in
         if kind == Identifier [] then let
               val (tok', file', sym) = parseId (tok, file)
               val (tok', file', id) = parseIdentifierSym (tok', file')
               val (tok', file') = checkTok (tok', file') [Mapsto]
               val (tok', file', expr) = parseExpr (tok', file')

               (* For the sym, we need to replace the default that parseId gave
                * us with the real subtable of the thing.
                *)
               val handler = Absyn.ExnHandler {sym=SOME (#1 sym, Symbol.EXN), id=id, expr=expr,
                                               symtab=Symbol.empty(), ty=Types.NONE_YET,
                                               pos=tokenPos tok}
            in
               if #3 tok' == Comma then let val (tok', file') = checkTok (tok', file') [Comma]
                                        in doParseExnLst (tok', file', handler::lst)
                                        end
               else (tok', file', SOME {handlers=rev lst, default=NONE, ty=Types.NONE_YET,
                                        pos=tokenPos tok})
            end
         else if kind == Else then handleElseBranch (tok, file, lst)
              else (tok, file, SOME {handlers=rev lst, default=NONE, ty=Types.NONE_YET,
                                     pos=tokenPos tok})
      end
   in
      doParseExnLst (tok, file, [])
   end

   (* expr = lparen-symbol base-expr rparen-symbol (handle-symbol exn-lst end-symbol)?
    *      | base-expr (handle-symbol exn-lst end-symbol)?
    *)
   and parseExpr (tok, file) = let
      (* base-expr = record-literal
       *           | lbrack-symbol expr-lst? rbrack-symbol
       *           | case-expr
       *           | decl-expr
       *           | if-expr
       *           | sym-ref
       *           | raise-symbol expr
       *           | integer-symbol
       *           | string-symbol
       *           | boolean-symbol
       *           | bottom-symbol
       *)
      fun parseBaseExpr (tok as (_, _, kind), file) =
         case kind of
            LBrace       => parseRecordLiteral (tok, file)
          | LBrack       => let
                               val (tok', file', lst) = wrappedLst (tok, file) ([LBrack], [RBrack]) parseExprLst
                            in
                               (tok', file', Absyn.ExprLstExp lst)
                            end
          | Case         => parseCaseExpr (tok, file)
          | Decl         => parseDeclExpr (tok, file)
          | If           => parseIfExpr (tok, file)
          | Identifier _ => parseSymRef (tok, file)
          | Raise        => let
                               val (tok', file') = checkTok (tok, file) [Raise]
                               val (tok', file', expr) = parseExpr (tok', file')
                            in
                               (tok', file', Absyn.RaiseExp expr)
                            end
          | Integer i    => let val (tok', file') = checkTok (tok, file) [Integer i]
                            in (tok', file', Absyn.IntegerExp i)
                            end
          | String s     => let val (tok', file') = checkTok (tok, file) [String s]
                            in (tok', file', Absyn.StringExp s)
                            end
          | Boolean b    => let val (tok', file') = checkTok (tok, file) [Boolean b]
                            in (tok', file', Absyn.BooleanExp b)
                            end
          | Bottom       => let val (tok', file') = checkTok (tok, file) [Bottom]
                            in (tok', file', Absyn.BottomExp)
                            end
          | _ => raise err (tok, [Boolean true, Bottom, Case, Decl, Identifier [], If, Integer 0,
                                  LBrace, LBrack, Raise, String []])

      (* case-expr = case-symbol expr in-symbol branch-lst end-symbol *)
      and parseCaseExpr (tok, file) = let
         (* branch-lst = branch-expr mapsto-symbol expr (comma-symbol branch-lst)?
          *            | else-symbol mapsto-symbol expr
          *)
         fun parseBranchLst (tok, file) = let
            fun doParseBranchLst (tok as (_, _, kind), file, lst) = let
               fun handleElseBranch (tok, file, lst) = let
                  val (tok', file') = checkTok (tok, file) [Else, Mapsto]
                  val (tok', file', expr) = parseExpr (tok', file')
               in
                  (tok', file', (rev lst, SOME expr))
               end
            in
               if inSet kind [Boolean true, Identifier [], Integer 0, String []] then let
                     val (tok', file', branch) = parseBranchExpr (tok, file)
                     val (tok', file') = checkTok (tok', file') [Mapsto]
                     val (tok', file', expr) = parseExpr (tok', file')
                  in
                     if #3 tok' == Comma then let val (tok', file') = checkTok (tok', file') [Comma]
                                              in doParseBranchLst (tok', file', (branch, expr)::lst)
                                              end
                     else (tok', file', (rev lst, NONE))
                  end
               else if kind == Else then handleElseBranch (tok, file, lst)
                    else (tok, file, (rev lst, NONE))
            end

            (* branch-expr = id
             *             | id lparen-symbol name-lst? rparen-symbol
             *             | integer-symbol
             *             | string-symbol
             *             | boolean-symbol
             *)
            and parseBranchExpr (tok as (_, _, kind), file) = let
               fun parseIdBranch (tok, file) = let
                  val (tok', file', sym) = parseId (tok, file)
               in
                  if #3 tok == LParen then let
                        val (tok', file', lst) = wrappedLst (tok', file') ([LParen], [RParen]) parseNameLst
                     in
                        (tok', file', Absyn.UnionBranch (sym, lst))
                     end
                  else
                     (tok', file', Absyn.RegularBranch (Absyn.IdExp sym))
               end
            in
               if inSet kind [Boolean true, Integer 0, String []] then let
                     val (tok', file', expr) = parseBaseExpr (tok, file)
                  in
                     (tok', file', Absyn.RegularBranch expr)
                  end
               else
                  if kind == Identifier [] then parseIdBranch (tok, file)
                  else raise err (tok, [Boolean true, Identifier [], Integer 0, String []]) 
            end
         in
            doParseBranchLst (tok, file, [])
         end

         val (tok', file') = checkTok (tok, file) [Case]
         val (tok', file', testExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') [In]
         val (tok', file', (branchLst, default)) = parseBranchLst (tok', file')
         val (tok', file') = checkTok (tok', file') [End]
      in
         (tok', file', Absyn.CaseExp{test=testExpr, default=default, branches=branchLst})
      end

      (* decl-expr = decl-symbol decl+ in-symbol expr end-symbol *)
      and parseDeclExpr (tok, file) = let
         fun parseDeclLst (tok as (_, _, kind), file, lst) =
            if inSet kind [Absorb, Function, Type, Val] then let
                  val (tok', file', ast) = parseDecl (tok, file)
               in
                  parseDeclLst (tok', file', ast::lst)
               end
            else if length lst = 0 then raise ParseError ("FIXME", #1 tok, #2 tok, "decl expressions must contain at least one declaration")
                 else (tok, file, rev lst)

         val (declTok, file') = checkTok (tok, file) [Decl]
         val (tok', file', decls) = parseDeclLst (declTok, file', [])
         val (exprTok, file') = checkTok (tok', file') [In]
         val (tok', file', expr) = parseExpr (exprTok, file')
         val (tok', file') = checkTok (tok', file') [End]
      in
         (tok', file', Absyn.DeclExp{decls=decls, expr=expr, symtab=Symbol.empty()})
      end

      (* if-expr = if-symbol expr then-symbol expr else-symbol expr *)
      and parseIfExpr (tok, file) = let
         val (tok', file') = checkTok (tok, file) [If]
         val (tok', file', testExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') [Then]
         val (tok', file', thenExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') [Else]
         val (tok', file', elseExpr) = parseExpr (tok', file')
      in
         (tok', file', Absyn.IfExp{test=testExpr, then'=thenExpr, else'=elseExpr})
      end

      fun doParseExpr (tok as (_, _, kind), file) =
         if kind == LParen then wrappedLst (tok, file) ([LParen], [RParen]) parseBaseExpr
         else parseBaseExpr (tok, file)

      val (tok', file', expr) = doParseExpr (tok, file)
   in
      if #3 tok' == Handle then let
            val (tok', file', lst) = wrappedLst (tok', file') ([Handle], [End]) parseExnLst
         in
            (tok', file', Absyn.Expr{expr=expr, pos=tokenPos tok, ty=Types.NONE_YET,
                                     exnHandler=lst})
         end
      else
         (tok', file', Absyn.Expr{expr=expr, pos=tokenPos tok, ty=Types.NONE_YET,
                                  exnHandler=NONE})
   end

   (* expr-lst = expr (comma-symbol expr)* *)
   and parseExprLst (tok, file) = let
      fun doParseExprLst (tok, file, lst) = let
         val (tok', file', expr) = parseExpr (tok, file)
         val lst' = expr::lst
      in
         case #3 tok' of
            Comma => let val (tok', file') = checkTok (tok', file') [Comma]
                     in doParseExprLst (tok', file', lst')
                     end
          | _     => (tok', file', rev lst')
      end
   in
      doParseExprLst (tok, file, [])
   end

   (* A subtle distinction between this and parseId.  This function just
    * returns the UniChar.Data from the Identifier token.  This can then be
    * wrapped up into whatever kind of symbol.  Use this when the production
    * takes a single name, not a dot-separated path.  Use parseId in that case.
    *)
   and parseIdentifierSym (tok as (_, _, kind), file) =
      case kind of
         Identifier i => let val (tok', file') = eat file
                         in (tok', file', i)
                         end
       | _            => raise err (tok, [Identifier[]])

   (* id = identifier-symbol (dot-symbol identifier-symbol)* *)
   and parseId (tok, file) = let
      fun doParseId (tok as (_, _, kind), file, lst) = let
         val (tok', file', id) = parseIdentifierSym (tok, file)
      in
         (* We don't know exactly where parseId is going to be called, so the
          * subtable is going to be wrong some of the time.  Callers will need
          * to modify this return value appropriately.
          *)
         if #3 tok' == Dot then let val (tok', file') = checkTok (tok', file') [Dot]
                                in doParseId (tok', file', (id, Symbol.mangle id)::lst)
                                end
         else (tok', file', (rev lst, Symbol.VALUE))
      end
   in
      doParseId (tok, file, [])
   end

   (* module-decl = module-symbol identifier-symbol assign-symbol decl-symbol top-decl+ end-symbol *)
   and parseModuleDecl (tok, file) = let
      (* top-decl = decl | module-decl *)
      fun parseTopDecl (tok as (_, _, kind), file, lst) =
         if kind == Module then let
               val (tok', file', ast) = parseModuleDecl (tok, file)
            in
               parseTopDecl (tok', file', ast::lst)
            end
         else if inSet kind [Absorb, Function, Type, Val] then let
                    val (tok', file', ast) = parseDecl (tok, file)
                 in
                    parseTopDecl (tok', file', ast::lst)
                 end
              else (tok, file, rev lst)

      val (idTok, file') = checkTok (tok, file) [Module]
      val (tok', file', id) = parseIdentifierSym (idTok, file')
      val (tok', file') = checkTok (idTok, file') [Identifier [], Assign, Decl]
      val (tok', file', declLst) = parseTopDecl (tok', file', [])
      val (tok', file') = checkTok (tok', file') [End]

      val sym = Symbol.toSymbol (id, Symbol.MODULE)
   in
      (tok', file', Absyn.ModuleDecl{sym=sym, decl=declLst, pos=tokenPos idTok,
                                     symtab=Symbol.empty()})
   end

   (* name-lst = identifier-symbol (comma-symbol identifier-symbol)* *)
   and parseNameLst (tok, file) = let
      fun doParseNameLst (tok, file, lst) = let
         val (tok', file', id) = parseIdentifierSym (tok, file)
         val sym = Symbol.toSymbol (id, Symbol.VALUE)
         val lst' = sym::lst
      in
         case #3 tok' of
            Comma => let val (tok', file') = checkTok (tok', file') [Comma]
                     in doParseNameLst (tok', file', lst')
                     end
          | _     => (tok', file', rev lst')
      end
   in
      doParseNameLst (tok, file, [])
   end

   and parseOptionalType (tok, file) =
      if #3 tok == Colon then let
            val (tyTok, file') = checkTok (tok, file) [Colon]
            val (tok', file', ty) = parseTy (tyTok, file')
         in
            (tok', file', SOME ty)
         end
      else
         (tok, file, NONE)

   (* record-literal = lbrace-symbol record-assn-lst rbrace-symbol *)
   and parseRecordLiteral (tok, file) = let
      (* record-assn-lst = identifier-symbol assign-symbol expr (comma-symbol record-assn-lst)* *)
      fun parseRecordAssnLst (tok, file) = let
         fun doParseRecordAssnLst (tok, file, lst) = let
            val (tok', file', id) = parseIdentifierSym (tok, file)
            val (exprTok, file') = checkTok (tok', file') [Assign]
            val (tok', file', expr) = parseExpr (exprTok, file')
            val sym = Symbol.toSymbol (id, Symbol.VALUE)
            val lst' = (sym, expr)::lst
         in
            case #3 tok' of
               Comma => doParseRecordAssnLst (tok', file', lst')
             | _     => (tok', file', rev lst')
         end
      in
         doParseRecordAssnLst (tok, file, [])
      end

      val (tok', file', lst) = wrappedLst (tok, file) ([LBrace], [RBrace]) parseRecordAssnLst
   in
      (tok', file', Absyn.RecordAssnExp lst)
   end

   (* sym-ref = id
    *         | id arg-lst
    *         | id arg-lst lparen-symbol ty-lst? rparen-symbol
    *         | id record-literal
    *         | sym-ref record-ref
    *)
   (* TODO *)
   and parseSymRef (tok, file) = let
      (* arg-lst = lparen-symbol expr-lst? rparen-symbol *)
      fun parseArgLst (tok, file) = 
         wrappedLst (tok, file) ([LParen], [RParen]) parseExprLst

      (* record-ref = (pipe-symbol identifier-symbol)+ *)
      (* TODO *)
      and parseRecordRef (tok, file) = ()

      (* ty-lst = ty (comma-symbol ty)* *)
      and parseTyLst (tok, file) = let
         fun doParseTyLst (tok, file, lst) = let
            val (tok', file', ty) = parseTy (tok, file)
            val lst' = ty::lst
         in
            case #3 tok of
               Comma => let val (tok', file') = checkTok (tok', file') [Comma]
                        in doParseTyLst (tok', file', lst')
                        end
             | _     => (tok', file', rev lst')
         end
      in
         doParseTyLst (tok, file, [])
      end
   in
      (tok, file, Absyn.BottomExp)
   end

   (* ty = bottom-symbol
    *    | exn-symbol lbrace-symbol typed-name-lst rbrace-symbol
    *    | id
    *    | lbrace-symbol typed-name-lst rbrace-symbol
    *    | list-symbol ty
    *    | union-symbol lbrace-symbol tycon-lst rbrace-symbol
    *)
   and parseTy (tok as (l, c, kind), file) = let
      fun parseExnTy (tok, file) = let
         val (tok', file', lst) = wrappedLst (tok, file) ([Exn, LBrace], [RBrace]) parseTypedNameLst
      in
         (tok', file', Absyn.ExnTy{exn'=lst, pos=tokenPos tok})
      end

      fun parseIdentifierTy (tok, file) = let
         val (tok', file', sym) = parseId (tok, file)
      in
         (tok', file', Absyn.IdTy{sym=sym, pos=tokenPos tok})
      end

      fun parseListTy (tok, file) = let
         val (tyTok, file') = checkTok (tok, file) [List]
         val (tok', file', ty) = parseTy (tyTok, file')
      in
         (tok', file', Absyn.ListTy{lst=ty, pos=tokenPos tok})
      end

      fun parseRecordTy (tok, file) = let
         val (tok', file', lst) = wrappedLst (tok, file) ([LBrace], [RBrace]) parseTypedNameLst
      in
         (tok', file', Absyn.RecordTy{record=lst, pos=tokenPos tok})
      end

      fun parseUnionTy (tok, file) = let
         (* tycon-lst = identifier-symbol (colon-symbol ty)? (comma-symbol tycon-lst)* *)
         fun parseTyconLst (tok, file) = let
            fun doParseTyconLst (tok, file, lst) = let
               val (tok', file', id) = parseIdentifierSym (tok, file)
               val (tok', file', ty) = parseOptionalType (tok', file')
               val sym = Symbol.toSymbol (id, Symbol.TYPE)
            in
               if #3 tok' == Comma then let
                     val (tok', file') = checkTok (tok', file') [Comma]
                  in
                     doParseTyconLst (tok', file', (sym, ty, tokenPos tok)::lst)
                  end
               else
                  (tok, file, rev lst)
            end
         in
            doParseTyconLst (tok, file, [])
         end

         val (tok', file', tycons) = wrappedLst (tok, file) ([Union, LBrace], [RBrace]) parseTyconLst
      in
         (tok', file', Absyn.UnionTy{tycons=tycons, pos=tokenPos tok})
      end
   in
      case kind of
         Bottom       => (tok, file, Absyn.BottomTy (l, c))
       | Exn          => parseExnTy (tok, file)
       | Identifier _ => parseIdentifierTy (tok, file)
       | LBrace       => parseRecordTy (tok, file)
       | List         => parseListTy (tok, file)
       | Union        => parseUnionTy (tok, file)
       | _            => raise err (tok, [Bottom, Exn, Identifier [], LBrace, List, Union])
   end

   (* typed-name-lst = identifier-symbol colon-symbol ty (comma-symbol identifier-symbol colon-symbol ty)* *)
   and parseTypedNameLst (tok, file) = let
      fun doParseTypedNameLst (tok, file, lst) = let
         val (tok', file', id) = parseIdentifierSym (tok, file)
         val (tyTok, file') = checkTok (tok', file') [Colon]
         val (tok', file', ty) = parseTy (tyTok, file)
         val sym = Symbol.toSymbol (id, Symbol.VALUE)
         val lst' = (sym, ty, tokenPos tok)::lst
      in
         case #3 tok' of
            Comma => let val (tok', file') = checkTok (tok', file') [Comma]
                     in doParseTypedNameLst (tok', file', lst')
                     end
          | _     => (tok', file', rev lst')
      end
   in
      doParseTypedNameLst (tok, file, [])
   end
end
