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
            Module => let
                         val (tok', file', ast) = parseModuleDecl (tok, file)
                      in
                         doParseStart (tok', file', ast::lst)
                      end
          | EndOfFile => (tok, file, rev lst)
          | _         => raise err (tok, [Module, EndOfFile])
   in
      doParseStart (tok, file, [])
   end

   (* decl = absorb-symbol id | fun-decl | ty-decl | val-decl *)
   and parseDecl (tok as (_, _, kind), file) = let
       fun parseOptionalType (tok, file) =
          if #3 tok == Colon then let
                val (tyTok, file') = checkTok (tok, file) [Colon]
                val (tok', file', ty) = parseTy (tyTok, file')
             in
                (tok', file', SOME ty)
             end
          else
             (tok, file, NONE)

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
         Absorb =>   parseAbsorbDecl (tok, file)
       | Function => parseFunDecl (tok, file)
       | Type =>     parseTyDecl (tok, file)
       | Val =>      parseValDecl (tok, file)
       | _ =>        raise err (tok, [Absorb, Function, Type, Val])
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
      fun parseBaseExpr (tok, file) = ()
(*
         case tok of
            (_, _, LBrace)       => parseRecordLiteral (tok, file)
          | (_, _, LBrack)       => (tok, file, [])
          | (_, _, Case)         => parseCaseExpr (tok, file)
          | (_, _, Decl)         => parseDeclExpr (tok, file)
          | (_, _, If)           => parseIfExpr (tok, file)
          | (_, _, Identifier _) => parseSymRef (tok, file)
          | (_, _, Raise)        => (tok, file, [])
          | (_, _, Integer i)    => let
                                       val (tok', file') = checkTok (tok, file) (Integer i)
                                    in
                                       (tok', file', Absyn.IntegerExp i)
                                    end
          | (_, _, String s)     => let
                                       val (tok', file') = checkTok (tok, file) (String s)
                                    in
                                       (tok', file', Absyn.StringExp s)
                                    end
          | (_, _, Boolean b)    => let
                                       val (tok', file') = checkTok (tok, file) (Boolean b)
                                    in
                                       (tok', file', Absyn.BooleanExp b)
                                    end
          | (_, _, Bottom)       => let
                                       val (tok', file') = checkTok (tok, file) Bottom
                                    in
                                       (tok', file', Absyn.BottomExp)
                                    end
*)

      (* branch-expr = id
       *             | id lparen-symbol name-lst? rparen-symbol
       *             | integer-symbol
       *             | string-symbol
       *             | boolean-symbol
       *)
      (* TODO *)
      and parseBranchExpr (tok, file) = ()

      (* case-expr = case-symbol expr in-symbol branch-lst end-symbol *)
      (* TODO *)
      and parseCaseExpr (tok, file) = let
         (* branch-lst = branch-expr mapsto-symbol expr (comma-symbol branch-lst)?
          *            | else-symbol mapsto-symbol expr
          *)
         fun parseBranchLst (tok, file) = (tok, file, [])

         val (tok', file') = checkTok (tok, file) [Case]
         val (tok', file', testExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') [In]
         val (tok', file', branchLst) = parseBranchLst (tok', file')
         val (tok', file') = checkTok (tok', file') [End]
      in
         ()
(*         (tok', file', Absyn.CaseExp{test=testExpr, default=NONE, branches=branchLst}) *)
      end

      (* decl-expr = decl-symbol decl+ in-symbol expr end-symbol *)
      (* TODO *)
      and parseDeclExpr (tok, file) = ()

      (* if-expr = if-symbol expr then-symbol expr else-symbol expr *)
      (* TODO *)
      and parseIfExpr (tok, file) = let
         val (tok', file') = checkTok (tok, file) [If]
         val (tok', file', testExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') [Then]
         val (tok', file', thenExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') [Else]
         val (tok', file', elseExpr) = parseExpr (tok', file')
      in
         ()
(*         (tok', file', Absyn.IfExp{test=testExpr, then'=thenExpr, else'=elseExpr}) *)
      end
   in
      (tok, file, Absyn.Expr{expr=Absyn.BottomExp, pos=(0, 0), ty=Types.NONE_YET, exnHandler=NONE})
   end

   (* expr-lst = expr (comma-symbol expr)* *)
   (* TODO *)
   and parseExprLst (tok, file) = ()

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
      case tok of
         (_, _, Identifier i) => doParseId (tok, file, [])
       | _                    => raise err (tok, [Identifier[]])
   end
*)
   and parseId (tok as (_, _, kind), file) =
      case tok of
         (_, _, Identifier i) => let val (tok', file') = eat file
                                 in (tok', file', Symbol.toSymbol (i, Symbol.VALUE))
                                 end
       | _ => raise err (tok, [Identifier[]])

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
            Comma => let
                        val (tok', file') = checkTok (tok', file') [Comma]
                     in
                        doParseNameLst (tok', file', lst')
                     end
          | _     => (tok', file', rev lst')
      end
   in
      doParseNameLst (tok, file, [])
   end

   (* record-literal = lbrace-symbol record-assn-lst rbrace-symbol *)
   (* TODO *)
   and parseRecordLiteral (tok, file) = let
      (* record-assn-lst = identifier-symbol assign-symbol expr (comma-symbol record-assn-lst)* *)
      (* TODO *)
      fun parseRecordAssnLst (tok, file) = ()
   in
      ()
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
      (* TODO *)
      fun parseArgLst (tok, file) = ()

      (* record-ref = (pipe-symbol identifier-symbol)+ *)
      (* TODO *)
      and parseRecordRef (tok, file) = ()

      (* ty-lst = ty (comma-symbol ty)* *)
      (* TODO *)
      and parseTyLst (tok, file) = ()
   in
      ()
   end

   (* ty = bottom-symbol
    *    | list-symbol ty
    *    | exn-symbol lbrace-symbol typed-name-lst rbrace-symbol
    *    | union-symbol lbrace-symbol tycon-lst rbrace-symbol
    *    | lbrace-symbol typed-name-lst rbrace-symbol
    *    | id
    *)
   (* TODO *)
   and parseTy (tok, file) = let
      (* tycon-lst = identifier-symbol colon-symbol ty (comma-symbol tycon-lst)*
       *           | identifier-symbol (comma-symbol tycon-lst)*
       *)
      (* TODO *)
      fun parseTyconLst (tok, file) = ()
   in
      (tok, file, Absyn.BottomTy(0, 0))
   end

   (* typed-name-lst = identifier-symbol colon-symbol ty (comma-symbol identifier-symbol colon-symbol ty)* *)
   and parseTypedNameLst (tok, file) = let
      fun doParseTypedNameLst (tok, file, lst) = let
         val (tok', file', sym) = parseId (tok, file)
         val (tyTok, file') = checkTok (tok', file') [Colon]
         val (tok', file', ty) = parseTy (tyTok, file)
         val lst' = (sym, ty)::lst
      in
         case #3 tok' of
            Comma => let
                        val (tok', file') = checkTok (tok', file') [Comma]
                     in
                        doParseTypedNameLst (tok', file', lst')
                     end
          | _     => (tok', file', rev lst')
      end
   in
      doParseTypedNameLst (tok, file, [])
   end
end
