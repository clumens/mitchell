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
      and parseFunDecl (tok, file) = let
         (* formals-lst = lparen-symbol typed-name-lst? rparen-symbol *)
         fun parseFormalsLst (tok, file) = let
            val (tok', file') = checkTok (tok, file) [LParen]
            val (tok', file', lst) = parseTypedNameLst (tok', file')
            val (tok', file') = checkTok (tok, file) [RParen]
         in
            (tok', file', lst)
         end

         (* ty-formals-lst = lparen-symbol name-lst? rparen-symbol *)
         and parseTyFormalsLst (tok, file) = let
            val (tok', file') = checkTok (tok, file) [LParen]
            val (tok', file', lst) = parseNameLst (tok', file')
            val (tok', file') = checkTok (tok, file) [RParen]
         in
            (tok', file', lst)
         end

         val (idTok, file') = checkTok (tok, file) [Function]
         val (tyFormalsTok, file', sym) = parseId (idTok, file')
         val (formalsTok, file', tyFormals) = parseTyFormalsLst (tyFormalsTok, file')
         val (tok', file', formals) = parseFormalsLst (formalsTok, file')
         val (tok', file', ty) = parseOptionalType (tok', file')
         val (exprTok, file') = checkTok (tok', file') [Assign]
         val (tok', file', expr) = parseExpr (exprTok, file')
      in
         (tok', file', Absyn.FunDecl{sym=sym, retval=ty, pos=tokenPos tok, formals=formals,
                                     tyFormals=tyFormals, calls=[], symtab=Symbol.empty(),
                                     body=expr})
      end

      (* ty-decl = type-symbol identifier-symbol assign-symbol ty *)
      and parseTyDecl (tok, file) = let
         val (idTok, file') = checkTok (tok, file) [Type]
         val (tok', file', sym) = parseId (idTok, file')
         val (tyTok, file') = checkTok (tok', file') [Assign]
         val (tok', file', ty) = parseTy (tyTok, file')
      in
         (tok', file', Absyn.TyDecl{sym=sym, ty=Types.NONE_YET, absynTy=ty, pos=tokenPos tok})
      end

      (* val-decl = val-symbol identifier-symbol (colon-symbol ty)? assign-symbol expr *)
      and parseValDecl (tok, file) = let
         val (idTok, file') = checkTok (tok, file) [Val]
         val (tok', file', sym) = parseId (idTok, file')
         val (tok', file', ty) = parseOptionalType (tok', file')
         val (exprTok, file') = checkTok (tok, file) [Assign]
         val (tok', file', expr) = parseExpr (exprTok, file')
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
   (* TODO *)
   and parseExnLst (tok, file) = ()

   (* expr = lparen-symbol base-expr rparen-symbol (handle-symbol exn-lst end-symbol)?
    *      | base-expr (handle-symbol exn-lst end-symbol)?
    *)
   (* TODO *)
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
      (* TODO *)
      fun parseBaseExpr (tok as (_, _, kind), file) =
         case kind of
            LBrace       => parseRecordLiteral (tok, file)
          | LBrack       => let
                               val (tok', file') = checkTok (tok, file) [LBrack]
                               val (tok', file', lst) = parseExprLst (tok, file)
                               val (tok', file') = checkTok (tok, file) [RBrack]
                            in
                               (tok', file', Absyn.ExprLstExp lst)
                            end
          | Case         => parseCaseExpr (tok, file)
          | Decl         => parseDeclExpr (tok, file)
          | If           => parseIfExpr (tok, file)
          | Identifier _ => parseSymRef (tok, file)
          | Raise        => (tok, file, Absyn.BottomExp)
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
      (* TODO *)
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
            (* TODO *)
            and parseBranchExpr (tok as (_, _, kind), file) = let
               fun parseIdBranch (tok, file) = let
                  val (tok', file', sym) = parseId (tok, file)
               in
                  if #3 tok == LParen then let
                        val (lstTok, file') = checkTok (tok', file') [LParen]
                        val (tok', file', lst) = parseNameLst (lstTok, file)
                        val (tok', file') = checkTok (tok', file') [RParen]
                     in
                        (tok', file', Absyn.UnionBranch (sym, lst))
                     end
                  else
                     (tok', file', Absyn.RegularBranch (Absyn.IdExp sym))
               end
            in
               if inSet kind [Boolean true, Integer 0, String []] then (tok, file, Absyn.RegularBranch (parseExpr tok))
               else if kind == Identifier [] then parseIdBranch (tok, file)
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
   in
      (tok, file, Absyn.Expr{expr=Absyn.BottomExp, pos=(0, 0), ty=Types.NONE_YET, exnHandler=NONE})
   end

   (* expr-lst = expr (comma-symbol expr)* *)
   (* TODO *)
   and parseExprLst (tok, file) = (tok, file, [])

   (* id = identifier-symbol (dot-symbol identifier-symbol)* *)
(*
   and parseId (tok as (_, _, kind), file) = let
      fun doParseId (tok, file, lst) = let
         val (tok', file') = checkTok (tok, file) [Identifier[]]
         val id = stripId kind
         val lst' = (id, Symbol.mangle id)::lst
      in
         (* This could also be a Symbol.TYPE.  How do we check that here? *)
         if kind == Dot then doParseId (tok', file', lst')
         else (tok', file', (rev lst', Symbol.VALUE))
      end
   in
      case kind of
         Identifier i => doParseId (tok, file, [])
       | _            => raise err (tok, [Identifier[]])
   end
*)
   and parseId (tok as (_, _, kind), file) =
      case kind of
         Identifier i => let val (tok', file') = eat file
                         in (tok', file', Symbol.toSymbol (i, Symbol.VALUE))
                         end
       | _            => raise err (tok, [Identifier[]])

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
      val (tok', file') = checkTok (idTok, file') [Identifier [], Assign, Decl]
      val (tok', file', declLst) = parseTopDecl (tok', file', [])
      val (tok', file') = checkTok (tok', file') [End]
   in
      (tok', file', Absyn.ModuleDecl{sym=Symbol.toSymbol (stripId (#3 idTok), Symbol.MODULE),
                                     decl=declLst, pos=tokenPos idTok, symtab=Symbol.empty()})
   end

   (* name-lst = identifier-symbol (comma-symbol identifier-symbol)* *)
   and parseNameLst (tok, file) = let
      fun doParseNameLst (tok, file, lst) = let
         val (tok', file', sym) = parseId (tok, file)
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
            val (tok', file', sym) = parseId (tok, file)
            val (exprTok, file') = checkTok (tok', file') [Assign]
            val (tok', file', expr) = parseExpr (exprTok, file')
            val lst' = (sym, expr)::lst
         in
            case #3 tok' of
               Comma => doParseRecordAssnLst (tok', file', lst')
             | _     => (tok', file', rev lst')
         end
      in
         doParseRecordAssnLst (tok, file, [])
      end

      val (lstTok, file') = checkTok (tok, file) [LBrace]
      val (tok', file', lst) = parseRecordAssnLst (lstTok, file')
      val (tok', file') = checkTok (tok', file') [RBrace]
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
      fun parseArgLst (tok, file) = let
         val (lstTok, file') = checkTok (tok, file) [LParen]
         val (tok', file', lst) = parseExprLst (lstTok, file')
         val (tok', file') = checkTok (tok, file) [RParen]
      in
         (tok', file', lst)
      end

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
         val (lstTok, file') = checkTok (tok, file) [Exn, LBrace]
         val (tok', file', lst) = parseTypedNameLst (lstTok, file')
         val (tok', file') = checkTok (tok', file') [RBrace]
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
         val (lstTok, file') = checkTok (tok, file) [LBrace]
         val (tok', file', lst) = parseTypedNameLst (lstTok, file')
         val (tok', file') = checkTok (tok', file') [RBrace]
      in
         (tok', file', Absyn.RecordTy{record=lst, pos=tokenPos tok})
      end

      fun parseUnionTy (tok, file) = let
         (* tycon-lst = identifier-symbol (colon-symbol ty)? (comma-symbol tycon-lst)* *)
         fun parseTyconLst (tok, file) = let
            fun doParseTyconLst (tok, file, lst) = let
               val (tok', file', sym) = parseId (tok, file)
               val (tok', file', ty) = parseOptionalType (tok', file')
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

         val (tyconTok, file') = checkTok (tok, file) [Union, LBrace]
         val (tok', file', tycons) = parseTyconLst (tyconTok, file')
         val (tok', file') = checkTok (tok', file') [RBrace]
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
         val (tok', file', sym) = parseId (tok, file)
         val (tyTok, file') = checkTok (tok', file') [Colon]
         val (tok', file', ty) = parseTy (tyTok, file)
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
