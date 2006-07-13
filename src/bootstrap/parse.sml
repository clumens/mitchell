signature PARSE =
sig
   val parse: Decode.DecFile -> unit list
end

structure Parse :> PARSE =
struct
   open Error
   open Tokens

   (* Read the next token from the token stream, returning the token and the
    * updated file object.
    *)
   fun eat file = nextToken file

   fun stripId (Identifier(id)) = id
     | stripId _ = raise Error.InternalError "symFromID passed something besides an Identifier"

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
          | _         => raise ParseError ("FIXME", line, col, "FIXME")
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
      fun parseBaseExpr (tok, file) = ()

      (* branch-lst = branch-expr mapsto-symbol expr (comma-symbol branch-lst)?
       *            | else-symbol mapsto-symbol expr
       *)
      and parseBranchLst (tok, file) = ()

      (* branch-expr = id
       *             | id lparen-symbol name-lst? rparen-symbol
       *             | integer-symbol
       *             | string-symbol
       *             | boolean-symbol
       *)
      and parseBranchExpr (tok, file) = ()

      (* case-expr = case-symbol expr in-symbol branch-lst end-symbol *)
      and parseCaseExpr (tok, file) = ()

      (* decl-expr = decl-symbol decl+ in-symbol expr end-symbol *)
      and parseDeclExpr (tok, file) = ()

      (* if-expr = if-symbol expr then-symbol expr else-symbol expr *)
      and parseIfExpr (tok, file) = ()
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
      fun parseTopDecl (tok, file) = ()

      val (idTok, file') = if (#3 tok) == Module then eat file
                           else raise ParseError ("", #1 tok, #2 tok, "")
      val (tok', file') = if (#3 idTok) == Identifier([]) then eat file
                          else raise ParseError ("", #1 idTok, #2 idTok, "")
      val (tok', file') = if (#3 tok') == Assign then eat file
                          else raise ParseError ("", #1 tok', #2 tok', "")
      val (tok', file') = if (#3 tok') == Decl then eat file
                          else raise ParseError ("", #1 tok', #2 tok', "")
   in
      (* ModuleDecl{sym=([stripId (#3 idTok), stripId (#3 idTok)], Subtable.MODULE),
       *            decl=[], pos=(#1 idTok, #2 idTok), symtab=false}
       *)
      (tok', file', ())
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
