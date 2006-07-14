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
   fun eat file = nextToken file


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
     | stripId _ = raise Error.InternalError "stripId given something besides an Identifier"

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
    fun checkTok (tok, file) expected =
       if (#3 tok) == expected then eat file
       else raise err (tok, [expected])


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
      fun doParseStart (tok as (line, col, t), file, lst) =
         case t of
            Module => let
                         val (tok', file', ast) = parseModuleDecl (tok, file)
                      in
                         doParseStart (tok', file', lst @ [ast])
                      end
          | EndOfFile => (tok, file, lst)
          | _         => raise err (tok, [Module, EndOfFile])
   in
      doParseStart (tok, file, [])
   end

   (* decl = ty-decl | val-decl | fun-decl | absorb-symbol id *)
   and parseDecl (tok, file) = let
      (* fun-decl = function-symbol identifier-symbol ty-formals-lst formals-lst (colon-symbol ty)? assign-symbol expr *)
      fun parseFunDecl (tok, file) = let
         (* formals-lst = lparen-symbol typed-name-lst? rparen-symbol *)
         fun parseFormalsLst (tok, file) = ()

         (* ty-formals-lst = lparen-symbol name-lst? rparen-symbol *)
         and parseTyFormalsLst (tok, file) = ()
      in
         ()
      end

      (* ty-decl = type-symbol identifier-symbol assign-symbol ty *)
      and parseTyDecl (tok, file) = ()

      (* val-decl = val-symbol identifier-symbol (colon-symbol ty)? assign-symbol expr *)
      and parseValDecl (tok, file) = ()
   in
      ()
   end

   (* exn-lst = id identifier-symbol mapsto-symbol expr (comma-symbol exn-lst)?
    *         | else-symbol identifier-symbol mapsto-symbol expr
    *)
   and parseExnLst (tok, file) = ()

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
      fun parseBaseExpr (tok, file) =
         case tok of
            (_, _, LBrace)       => parseRecordLiteral (tok, file)
          | (_, _, LBrack)       => (tok, file, [])
          | (_, _, Case)         => parseCaseExpr (tok, file)
          | (_, _, Decl)         => parseDeclExpr (tok, file)
          | (_, _, If)           => parseIfExpr (tok, file)
          | (_, _, Identifier _) => parseSymRef (tok, file)
          | (_, _, Raise)        => parseRaiseExpr (tok, file)
          | (_, _, Integer i)    => let
                                       val (tok', file') = checkTok (tok, file) (Integer 0)
                                    in
                                       (tok', file', Absyn.IntegerExp i)
                                    end
          | (_, _, String s)     => let
                                       val (tok', file') = checkTok (tok, file) (String [])
                                    in
                                       (tok', file', Absyn.StringExp s)
                                    end
          | (_, _, Boolean b)    => let
                                       val (tok', file') = checkTok (tok, file) (Boolean true)
                                    in
                                       (tok', file', Absyn.BooleanExp b)
                                    end
          | (_, _, Bottom)       => let
                                       val (tok', file') = checkTok (tok, file) Bottom
                                    in
                                       (tok', file', Absyn.BottomExp)
                                    end

      (* branch-expr = id
       *             | id lparen-symbol name-lst? rparen-symbol
       *             | integer-symbol
       *             | string-symbol
       *             | boolean-symbol
       *)
      and parseBranchExpr (tok, file) = ()

      (* case-expr = case-symbol expr in-symbol branch-lst end-symbol *)
      and parseCaseExpr (tok, file) = let
         (* branch-lst = branch-expr mapsto-symbol expr (comma-symbol branch-lst)?
          *            | else-symbol mapsto-symbol expr
          *)
         fun parseBranchLst (tok, file) = (tok, file, [])

         val (tok', file') = checkTok (tok, file) Case
         val (tok', file', testExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') In
         val (tok', file', branchLst) = parseBranchLst (tok', file')
         val (tok', file') = checkTok (tok', file') End
      in
         (tok', file', Absyn.CaseExp{test=testExpr, default=NONE, branches=branchLst})
      end

      (* decl-expr = decl-symbol decl+ in-symbol expr end-symbol *)
      and parseDeclExpr (tok, file) = ()

      (* if-expr = if-symbol expr then-symbol expr else-symbol expr *)
      and parseIfExpr (tok, file) = let
         val (tok', file') = checkTok (tok, file) If
         val (tok', file', testExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') Then
         val (tok', file', thenExpr) = parseExpr (tok', file')
         val (tok', file') = checkTok (tok', file') Else
         val (tok', file', elseExpr) = parseExpr (tok', file')
      in
         (tok', file', Absyn.IfExp{test=testExpr, then'=thenExpr, else'=elseExpr})
      end
   in
      ()
   end

   (* expr-lst = expr (comma-symbol expr)* *)
   and parseExprLst (tok, file) = ()

   (* id = identifier-symbol (dot-symbol identifier-symbol)* *)
   and parseId (tok, file) = ()

   (* module-decl = module-symbol identifier-symbol assign-symbol decl-symbol top-decl+ end-symbol *)
   and parseModuleDecl (tok, file) = let
      (* top-decl = decl | module-decl *)
      fun parseTopDecl (tok, file) =
         (* FIXME - dummy for now *)
         (tok, file, [Absyn.Absorb{module=([], Symbol.NONE), pos=(0, 0)}])

      val (idTok, file') = checkTok (tok, file) Module
      val (tok', file') = checkTok (idTok, file') (Identifier [])
      val (tok', file') = checkTok (tok', file') Assign
      val (tok', file') = checkTok (tok', file') Decl
      val (tok', file', declLst) = parseTopDecl (tok', file')
      val (tok', file') = checkTok (tok', file') End
   in
      (tok', file', Absyn.ModuleDecl{sym=Symbol.toSymbol (stripId (#3 idTok), Symbol.MODULE),
                                     decl=declLst, pos=(#1 idTok, #2 idTok),
                                     symtab=Symbol.empty()})
   end

   (* name-lst = identifier-symbol (comma-symbol identifier-symbol)* *)
   and parseNameLst (tok, file) = ()

   (* record-literal = lbrace-symbol record-assn-lst rbrace-symbol *)
   and parseRecordLiteral (tok, file) = let
      (* record-assn-lst = identifier-symbol assign-symbol expr (comma-symbol record-assn-lst)* *)
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
   and parseSymRef (tok, file) = let
      (* arg-lst = lparen-symbol expr-lst? rparen-symbol *)
      fun parseArgLst (tok, file) = ()

      (* record-ref = (pipe-symbol identifier-symbol)+ *)
      and parseRecordRef (tok, file) = ()

      (* ty-lst = ty (comma-symbol ty)* *)
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
   and parseTy (tok, file) = let
      (* tycon-lst = identifier-symbol colon-symbol ty (comma-symbol tycon-lst)*
       *           | identifier-symbol (comma-symbol tycon-lst)*
       *)
      fun parseTyconLst (tok, file) = ()
   in
      ()
   end

   (* typed-name-lst = identifier-symbol colon-symbol ty (comma-symbol typed-name-lst)* *)
   and parseTypedNameLst (tok, file) = ()
end
