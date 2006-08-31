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

      val acceptedStr = String.concatWith " " (map Tokens.toString (map asToken accepted))
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
   fun checkTok state lst = let
      fun doCheck (state as (tok, file)) expected =
         if #3 tok == expected then eat file
         else raise err (tok, [expected])

      fun loop state (expected::[]) = doCheck state expected
        | loop state (expected::lst') = loop (doCheck state expected) lst'
        | loop state [] = raise InternalError "checkTok called with empty list"
   in
      loop state lst
   end

   (* Extract position information from a token. *)
   fun tokenPos (tok:Tokens) = (#1 tok, #2 tok)

   (* Extract position information from a state. *)
   fun statePos ((tok, _):State) = tokenPos tok


   (* PARSING HELPER FUNCTIONS *)

   (* Consume the tokens surrounding a list and parse the list itself.  The
    * list is parsed by the function f.  The list may or may not be null,
    * depending on how f is written.
    *)
   fun wrappedLst state (openKind, closeKind) f = let
      val (state', ast) = f (checkTok state [openKind])
      val state' = checkTok state' [closeKind]
   in
      (state', ast)
   end

   (* Wrapper for a list parsing function f that would usually be passed to
    * wrappedLst above.  This function allows those lists to be empty.  Use
    * by passing the first two arguments, then using that call as the final
    * argument to wrappedLst.  Hooray currying.
    *)
   fun lstMayBeEmpty closeKind f (state as (tok:Tokens, _)) =
      if #3 tok == closeKind then (state, [])
      else f state

   (* Generic function for parsing a list of elements.  Provide the starting
    * list, the element separator, and a function to parse one element.  The
    * repeated results of calling function f are accumulated into a list, which
    * is reversed when a token is seen besides the separator.
    *)
   fun parseLst state lst sep f = let
      val (state' as (tok', _), ret) = f state
      val lst' = ret::lst
   in
      if #3 tok' == sep then parseLst (checkTok state' [sep]) lst' sep f
      else (state', rev lst')
   end

   (* Similar to parseLst, but allow for a default (or else) branch as part of
    * the list.  acceptSet is the list of tokens that defines the start of a
    * branch.  f is a function for parsing one branch, while e is the function
    * that parses the default.  Each of these functions must return an updated
    * file state and an element.  The default element will get shoved into an
    * option type.
    *)
   fun parseDefaultLst (state as (tok, _)) lst acceptSet sep f e = let
   in
      if inSet (#3 tok) acceptSet then let
            val (state' as (tok', _), ele) = f state
            val lst' = ele::lst
         in
            if #3 tok' == sep then parseDefaultLst (checkTok state' [sep]) lst' acceptSet sep f e
            else (state', (rev lst', NONE))
         end
      else if #3 tok == Else then let val (state', default) = e (checkTok state [Else])
                                  in (state', (rev lst, SOME default))
                                  end
           else (state, (rev lst, NONE))
   end


   (* PARSING FUNCTIONS *)

   (* Structure entry point - start parsing the token stream, returning an
    * abstract syntax tree.  We go through a couple hoops here because of
    * wanting to treat the start symbol correctly.
    *)
   fun parse file = let
      (* Prime the process by reading the first token from the input. *)
      val state = eat file
   in
      #2 (parseStart state)
   end

   (* start = module-decl+ *)
   and parseStart state = let
      fun doParseStart (state as (tok, _), lst) =
         case #3 tok of
            Module    => let val (state', ast) = parseModuleDecl state
                         in doParseStart (state', ast::lst)
                         end
          | EndOfFile => (state, rev lst)
          | _         => raise err (tok, [Module, EndOfFile])
   in
      doParseStart (state, [])
   end

   (* decl = absorb-symbol id | fun-decl | ty-decl | val-decl *)
   and parseDecl (state as (tok, _)) = let
      fun parseAbsorbDecl state = let
         val (state', sym) = parseId (checkTok state [Absorb])
      in
         (state', Absyn.Absorb{module=sym, pos=statePos state})
      end

      (* fun-decl = function-symbol identifier-symbol ty-formals-lst formals-lst (colon-symbol ty)? assign-symbol expr *)
      (* formals-lst = lparen-symbol typed-name-lst? rparen-symbol *)
      (* ty-formals-lst = lparen-symbol name-lst? rparen-symbol *)
      and parseFunDecl state = let
         val (state', id) = parseIdentifierSym (checkTok state [Function])
         val (state', tyFormals) = wrappedLst state' (LParen, RParen)
                                              (lstMayBeEmpty RParen parseNameLst)
         val (state', formals) = wrappedLst state' (LParen, RParen)
                                            (lstMayBeEmpty RParen parseTypedNameLst)
         val (state', ty) = parseOptionalType state'
         val (state', expr) = parseExpr (checkTok state' [Assign])
      in
         (state', Absyn.FunDecl{sym=Symbol.toSymbol (id, Symbol.FUNCTION), retval=ty,
                                pos=statePos state, formals=formals, tyFormals=tyFormals,
                                calls=[], symtab=Symbol.empty(), body=expr})
      end

      (* ty-decl = type-symbol identifier-symbol assign-symbol ty *)
      and parseTyDecl state = let
         val (state', id) = parseIdentifierSym (checkTok state [Type])
         val (state', ty) = parseTy (checkTok state' [Assign])
      in
         (state', Absyn.TyDecl{sym=Symbol.toSymbol (id, Symbol.TYPE), ty=Types.NONE_YET,
                               absynTy=ty, pos=statePos state})
      end

      (* val-decl = val-symbol identifier-symbol (colon-symbol ty)? assign-symbol expr *)
      and parseValDecl state = let
         val (state', id) = parseIdentifierSym (checkTok state [Val])
         val (state', ty) = parseOptionalType state'
         val (state', expr) = parseExpr (checkTok state' [Assign])
      in
         (state', Absyn.ValDecl{sym=Symbol.toSymbol (id, Symbol.VALUE), ty=Types.NONE_YET,
                                absynTy=ty, pos=statePos state, init=expr})
      end
   in
      case #3 tok of
         Absorb   => parseAbsorbDecl state
       | Function => parseFunDecl state
       | Type     => parseTyDecl state
       | Val      => parseValDecl state
       | _        => raise err (tok, [Absorb, Function, Type, Val])
   end

   (* exn-lst = id identifier-symbol mapsto-symbol expr (comma-symbol exn-lst)?
    *         | else-symbol identifier-symbol mapsto-symbol expr
    *)
   and parseExnLst (state as (tok, _)) = let
      fun handleElseBranch state = let
         val (state', id) = parseIdentifierSym state
         val state' = checkTok state' [Mapsto]
         val (state', expr) = parseExpr state'
      in
         (state', Absyn.ExnHandler {sym=NONE, id=id, expr=expr, symtab=Symbol.empty(),
                                     ty=Types.NONE_YET, pos=statePos state})
      end

      fun parseOneExn state = let
         val (state', sym) = parseId state
         val (state', id) = parseIdentifierSym state'
         val (state', expr) = parseExpr (checkTok state' [Mapsto])
      in
         (* For the sym, we need to replace the default that parseId gave us
          * with the real subtable of the thing.
          *)
         (state', Absyn.ExnHandler {sym=SOME (#1 sym, Symbol.EXN), id=id, expr=expr,
                                     symtab=Symbol.empty(), ty=Types.NONE_YET,
                                     pos=statePos state})
      end

      val (state', (lst, default)) = parseDefaultLst state [] [Identifier[]]
                                                     Comma parseOneExn handleElseBranch
   in
      if length lst = 0 andalso not (Option.isSome default) then raise ParseError ("FIXME", #1 tok, #2 tok, "exception handlers must have at least one branch or else clause")
      else (state', SOME {handlers=lst, default=default, ty=Types.NONE_YET, pos=statePos state})
   end

   (* expr = lparen-symbol base-expr rparen-symbol (handle-symbol exn-lst end-symbol)?
    *      | base-expr (handle-symbol exn-lst end-symbol)?
    *)
   and parseExpr state = let
      (* base-expr = record-literal
       *           | lbrack-symbol expr-lst? (brack-symbol
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
      fun parseBaseExpr (state as (tok, _)) =
         case #3 tok of
            LBrace       => parseRecordLiteral state
          | LBrack       => let
                               val (state', lst) = wrappedLst state (LBrack, RBrack)
                                                              (lstMayBeEmpty RBrack parseExprLst)
                            in
                               (state', Absyn.ExprLstExp lst)
                            end
          | Case         => parseCaseExpr state
          | Decl         => parseDeclExpr state
          | If           => parseIfExpr state
          | Identifier _ => parseSymRef state
          | Raise        => let val (state', expr) = parseExpr (checkTok state [Raise])
                            in (state', Absyn.RaiseExp expr)
                            end
          | Integer i    => (checkTok state [Integer i], Absyn.IntegerExp i)
          | String s     => (checkTok state [String s], Absyn.StringExp s)
          | Boolean b    => (checkTok state [Boolean b], Absyn.BooleanExp b)
          | Bottom       => (checkTok state [Bottom], Absyn.BottomExp)
          | _ => raise err (tok, [Boolean true, Bottom, Case, Decl, Identifier [], If, Integer 0,
                                  LBrace, LBrack, Raise, String []])

      (* case-expr = case-symbol expr in-symbol branch-lst end-symbol *)
      and parseCaseExpr (state as (tok, _)) = let
         (* branch-lst = branch-expr mapsto-symbol expr (comma-symbol branch-lst)?
          *            | else-symbol mapsto-symbol expr
          *)
         fun parseBranchLst state = let
            fun handleElseBranch state =
               parseExpr (checkTok state [Mapsto])

            (* branch-expr = id
             *             | id lparen-symbol name-lst? rparen-symbol
             *             | integer-symbol
             *             | string-symbol
             *             | boolean-symbol
             *)
            fun parseBranchExpr (state as (tok, _)) = let
               fun parseIdBranch state = let
                  val (state' as (tok', _), sym) = parseId state
               in
                  if #3 tok' == LParen then let
                        val (state', lst) = wrappedLst state' (LParen, RParen)
                                                       (lstMayBeEmpty RParen parseNameLst)
                     in
                        (state', Absyn.UnionBranch (sym, lst))
                     end
                  else
                     (state', Absyn.RegularBranch (Absyn.IdExp sym))
               end
            in
               if inSet (#3 tok) [Boolean true, Integer 0, String []] then let
                     val (state', expr) = parseBaseExpr state
                  in
                     (state', Absyn.RegularBranch expr)
                  end
               else
                  if (#3 tok) == Identifier [] then parseIdBranch state
                  else raise err (tok, [Boolean true, Identifier [], Integer 0, String []]) 
            end

            fun parseOneBranch state = let
               val (state', branch) = parseBranchExpr state
               val (state', expr) = parseExpr (checkTok state' [Mapsto])
            in
               (state', (branch, expr))
            end
         in
            parseDefaultLst state [] [Boolean true, Identifier [], Integer 0, String []]
                            Comma parseOneBranch handleElseBranch
         end

         val (state', testExpr) = parseExpr (checkTok state [Case])
         val (state', (branchLst, default)) = parseBranchLst (checkTok state' [In])
      in
         if length branchLst = 0 andalso not (Option.isSome default) then raise ParseError ("FIXME", #1 tok, #2 tok, "case expressions must have at least one branch or else clause")
         else (checkTok state' [End], Absyn.CaseExp{test=testExpr, default=default,
                                                    branches=branchLst})
      end

      (* decl-expr = decl-symbol decl+ in-symbol expr end-symbol *)
      and parseDeclExpr state = let
         fun parseDeclLst (state as (tok, _), lst) =
            if inSet (#3 tok) [Absorb, Function, Type, Val] then let
                  val (state', ast) = parseDecl state
               in
                  parseDeclLst (state', ast::lst)
               end
            else if #3 tok == Module then raise ParseError ("FIXME", #1 tok, #2 tok, "decl expressions may not define modules")
                 else if length lst = 0 then raise ParseError ("FIXME", #1 tok, #2 tok, "decl expressions must contain at least one declaration")
                      else (state, rev lst)

         val (state', decls) = parseDeclLst (checkTok state [Decl], [])
         val (state', expr) = parseExpr (checkTok state' [In])
         val state' = checkTok state' [End]
      in
         (state', Absyn.DeclExp{decls=decls, expr=expr, symtab=Symbol.empty()})
      end

      (* if-expr = if-symbol expr then-symbol expr else-symbol expr *)
      and parseIfExpr state = let
         val (state', testExpr) = parseExpr (checkTok state [If])
         val (state', thenExpr) = parseExpr (checkTok state' [Then])
         val (state', elseExpr) = parseExpr (checkTok state' [Else])
      in
         (state', Absyn.IfExp{test=testExpr, then'=thenExpr, else'=elseExpr})
      end

      fun doParseExpr (state as (tok, _)) =
         if (#3 tok) == LParen then wrappedLst state (LParen, RParen) parseBaseExpr
         else parseBaseExpr state

      val (state' as (tok', _), expr) = doParseExpr state
      val (state', lst) = if #3 tok' == Handle then wrappedLst state' (Handle, End) parseExnLst
                          else (state', NONE)
   in
      (state', Absyn.Expr{expr=expr, pos=statePos state, ty=Types.NONE_YET, exnHandler=lst})
   end

   (* expr-lst = expr (comma-symbol expr)* *)
   and parseExprLst state =
      parseLst state [] Comma parseExpr

   (* A subtle distinction between this and parseId.  This function just
    * returns the UniChar.Data from the Identifier token.  This can then be
    * wrapped up into whatever kind of symbol.  Use this when the production
    * takes a single name, not a dot-separated path.  Use parseId in that case.
    *)
   and parseIdentifierSym (state as (tok, file)) =
      case #3 tok of
         Identifier i => (eat file, i)
       | _            => raise err (tok, [Identifier[]])

   (* id = identifier-symbol (dot-symbol identifier-symbol)* *)
   and parseId state = let
      fun doParseId (state, lst) = let
         val (state' as (tok, _), id) = parseIdentifierSym state
      in
         (* We don't know exactly where parseId is going to be called, so the
          * subtable is going to be wrong some of the time.  Callers will need
          * to modify this return value appropriately.
          *)
         if #3 tok == Dot then doParseId (checkTok state' [Dot], (id, Symbol.mangle id)::lst)
         else (state', (rev lst, Symbol.VALUE))
      end
   in
      doParseId (state, [])
   end

   (* module-decl = module-symbol identifier-symbol assign-symbol decl-symbol top-decl+ end-symbol *)
   and parseModuleDecl (state as (tok, _)) = let
      (* top-decl = decl | module-decl *)
      fun parseTopDecl (state as (tok, _), lst) =
         if #3 tok == Module then let
               val (state', ast) = parseModuleDecl state
            in
               parseTopDecl (state', ast::lst)
            end
         else if inSet (#3 tok) [Absorb, Function, Type, Val] then let
                    val (state', ast) = parseDecl state
                 in
                    parseTopDecl (state', ast::lst)
                 end
              else (state, rev lst)

      val (state', id) = parseIdentifierSym (checkTok state [Module])
      val (state', declLst) = parseTopDecl (checkTok state' [Assign, Decl], [])
      val state' = checkTok state' [End]
   in
      if length declLst = 0 then raise ParseError ("FIXME", #1 tok, #2 tok, "modules must have at least one declaration")
      else (state', Absyn.ModuleDecl{sym=Symbol.toSymbol (id, Symbol.MODULE), decls=declLst,
                                     pos=statePos state, symtab=Symbol.empty()})
   end

   (* name-lst = identifier-symbol (comma-symbol identifier-symbol)* *)
   and parseNameLst state = let
      fun parseOneName state = let
         val (state', id) = parseIdentifierSym state
      in
         (state', Symbol.toSymbol (id, Symbol.VALUE))
      end
   in
      parseLst state [] Comma parseOneName
   end

   and parseOptionalType (state as (tok, _)) = 
      if #3 tok == Colon then let
            val (state', ty) = parseTy (checkTok state [Colon])
         in
            (state', SOME ty)
         end
      else
         (state, NONE)

   (* record-literal = lbrace-symbol record-assn-lst rbrace-symbol *)
   and parseRecordLiteral state = let
      (* record-assn-lst = identifier-symbol assign-symbol expr (comma-symbol record-assn-lst)* *)
      fun parseRecordAssnLst state = let
         fun parseOneRecordAssn state = let
            val (state', id) = parseIdentifierSym state
            val (state', expr) = parseExpr (checkTok state' [Assign])
         in
            (state', (Symbol.toSymbol (id, Symbol.VALUE), expr))
         end
      in
         parseLst state [] Comma parseOneRecordAssn
      end

      val (state', lst) = wrappedLst state (LBrace, RBrace) parseRecordAssnLst
   in
      (state', Absyn.RecordAssnExp lst)
   end

   (* sym-ref = id
    *         | id lparen-symbol ty-lst? rparen-symbol lparen-symbol expr-lst? rparen-symbol
    *         | id record-literal
    *         | sym-ref record-ref
    *)
   and parseSymRef state = let
      (* record-ref = (pipe-symbol identifier-symbol)+ *)
      fun parseRecordRef state = let
         fun doParseRecordRef (state, lst) = let
            val (state' as (tok, _), id) = parseIdentifierSym (checkTok state [Pipe])
            val lst' = (id, Symbol.mangle id)::lst
         in
            if #3 tok == Pipe then doParseRecordRef (state', lst')
            else (state', rev lst')
         end
      in
         doParseRecordRef (state, [])
      end

      fun doParseSymRef state = let
         (* ty-lst = ty (comma-symbol ty)* *)
         fun parseTyLst state =
            parseLst state [] Comma parseTy

         fun parseFunctionCall (state, name) = let
             val (state', tyLst) = wrappedLst state (LParen, RParen) (lstMayBeEmpty RParen parseTyLst)
             val (state', argLst) = wrappedLst state' (LParen, RParen) (lstMayBeEmpty RParen parseExprLst)
         in
             (state', Absyn.FunCallExp{function=name, args=argLst, tyArgs=tyLst, frees=[]})
         end

         fun parseExnExpr (state, name) = let
             val (state', r) = parseRecordLiteral state
             val lst = case r of Absyn.RecordAssnExp l => l
                               | _ => raise InternalError "parseRecordLiteral returned something besides a RecordAssnExp"
         in
             (state', Absyn.ExnExp{sym=name, ty=Types.NONE_YET, values=lst})
         end

         val (state' as (tok, file), sym) = parseId state
      in
         case #3 tok of
            LParen => parseFunctionCall (state', sym)
          | LBrace => parseExnExpr (state', sym)
          | _      => (state', Absyn.IdExp sym)
      end

      val (state' as (tok, _), expr) = doParseSymRef state
   in
      if #3 tok == Pipe then let
            val (state', ele) = parseRecordRef state'
         in
            (state', Absyn.RecordRefExp{record=expr, ele=(ele, Symbol.VALUE)})
         end
      else
         (state', expr)
   end

   (* ty = bottom-symbol
    *    | exn-symbol lbrace-symbol typed-name-lst rbrace-symbol
    *    | id
    *    | lbrace-symbol typed-name-lst rbrace-symbol
    *    | list-symbol ty
    *    | union-symbol lbrace-symbol tycon-lst rbrace-symbol
    *)
   and parseTy (state as (tok, _)) = let
      fun parseExnTy state = let
         val (state', lst) = wrappedLst (checkTok state [Exn]) (LBrace, RBrace) parseTypedNameLst
      in
         (state', Absyn.ExnTy{exn'=lst, pos=statePos state})
      end

      fun parseIdentifierTy state = let
         val (state', sym) = parseId state
      in
         (state', Absyn.IdTy{sym=sym, pos=statePos state})
      end

      fun parseListTy state = let
         val (state', ty) = parseTy (checkTok state [List])
      in
         (state', Absyn.ListTy{lst=ty, pos=statePos state})
      end

      fun parseRecordTy state = let
         val (state', lst) = wrappedLst state (LBrace, RBrace) parseTypedNameLst
      in
         (state', Absyn.RecordTy{record=lst, pos=statePos state})
      end

      fun parseUnionTy state = let
         (* tycon-lst = identifier-symbol (colon-symbol ty)? (comma-symbol tycon-lst)* *)
         fun parseTyconLst state = let
            fun parseOneTycon state = let
               val (state', id) = parseIdentifierSym state
               val (state', ty) = parseOptionalType state'
            in
               (state', (Symbol.toSymbol (id, Symbol.TYPE), ty, statePos state))
            end
         in
            parseLst state [] Comma parseOneTycon
         end

         val (state', tycons) = wrappedLst (checkTok state [Union]) (LBrace, RBrace) parseTyconLst
      in
         (state', Absyn.UnionTy{tycons=tycons, pos=statePos state})
      end
   in
      case #3 tok of
         Bottom       => (state, Absyn.BottomTy (tokenPos tok))
       | Exn          => parseExnTy state
       | Identifier _ => parseIdentifierTy state
       | LBrace       => parseRecordTy state
       | List         => parseListTy state
       | Union        => parseUnionTy state
       | _            => raise err (tok, [Bottom, Exn, Identifier [], LBrace, List, Union])
   end

   (* typed-name-lst = identifier-symbol colon-symbol ty (comma-symbol identifier-symbol colon-symbol ty)* *)
   and parseTypedNameLst state = let
      fun parseOneTypedName state = let
         val (state', id) = parseIdentifierSym state
         val (state', ty) = parseTy (checkTok state' [Colon])
      in
         (state', (Symbol.toSymbol (id, Symbol.VALUE), ty, statePos state))
      end
   in
      parseLst state [] Comma parseOneTypedName
   end
end
