(* Tests the tokenizing code by repeatedly fetching tokens and printing their
 * to stdout.
 *
 * Usage: readFile <filename>
 *)
CM.make ("../sources.cm");

fun openFile filename =
   Decode.decOpenUni (SOME(Uri.String2Uri filename), Encoding.UTF8)

fun printToken (tok:Tokens.tokens) =
   case (#3 tok) of
      Tokens.Absorb        => print "ABSORB\n"
    | Tokens.Assign        => print "ASSIGN\n"
    | Tokens.Boolean(v)    => print ("BOOLEAN(" ^ (if v then "true)\n" else "false)\n"))
    | Tokens.Bottom        => print "BOTTOM\n"
    | Tokens.Case          => print "CASE\n"
    | Tokens.Colon         => print "COLON\n"
    | Tokens.Comma         => print "COMMA\n"
    | Tokens.Dblquote      => print "DBLQUOTE\n"
    | Tokens.Decl          => print "DECL\n"
    | Tokens.Dot           => print "DOT\n"
    | Tokens.Else          => print "ELSE\n"
    | Tokens.End           => print "END\n"
    | Tokens.EndOfFile     => print "ENDOFFILE\n"
    | Tokens.Exn           => print "EXN\n"
    | Tokens.Function      => print "FUNCTION\n"
    | Tokens.Handle        => print "HANDLE\n"
    | Tokens.Identifier(v) => print ("IDENTIFIER(" ^ UniChar.Data2String v ^ ")\n")
    | Tokens.If            => print "IF\n"
    | Tokens.In            => print "IN\n"
    | Tokens.Integer(v)    => print ("INTEGER(" ^ Int.toString v ^ ")\n")
    | Tokens.LBrace        => print "LBRACE\n"
    | Tokens.LBrack        => print "LBRACK\n"
    | Tokens.List          => print "LIST\n"
    | Tokens.LParen        => print "LPAREN\n"
    | Tokens.Mapsto        => print "MAPSTO\n"
    | Tokens.Module        => print "MODULE\n"
    | Tokens.Pipe          => print "PIPE\n"
    | Tokens.Raise         => print "RAISE\n"
    | Tokens.RBrace        => print "RBRACE\n"
    | Tokens.RBrack        => print "RBRACK\n"
    | Tokens.RParen        => print "RPAREN\n"
    | Tokens.String(v)     => print ("STRING(" ^ UniChar.Data2String v ^ ")\n")
    | Tokens.Then          => print "THEN\n"
    | Tokens.Type          => print "TYPE\n"
    | Tokens.Union         => print "UNION\n"
    | Tokens.Val           => print "VAL\n"

fun readFile filename = let
   fun doRead file = let
      val (tok, file') = Tokens.nextToken file
   in
      case (#3 tok) of
         Tokens.EndOfFile => ()
       | _  => ( printToken tok ; doRead file' )
   end
in
   doRead (openFile filename)
end
