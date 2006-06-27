(* Tests the tokenizing code by repeatedly fetching tokens and printing their
 * to stdout.
 *
 * Usage: readFile <filename>
 *)
CM.make("../sources.cm");

fun openFile filename =
   Decode.decOpenUni(SOME(Uri.String2Uri(filename)), Encoding.UTF8)

fun printToken tok =
   case tok of
      Tokens.Assign(_, _)        => print "ASSIGN\n"
    | Tokens.Boolean(_, _, v)    => print ("BOOLEAN(" ^ (if v then "true)\n" else "false)\n"))
    | Tokens.Bottom(_, _)        => print "BOTTOM\n"
    | Tokens.Case(_, _)          => print "CASE\n"
    | Tokens.Colon(_, _)         => print "COLON\n"
    | Tokens.Comma(_, _)         => print "\n"
    | Tokens.Dblquote(_, _)      => print "DBLQUOTE\n"
    | Tokens.Decl(_, _)          => print "DECL\n"
    | Tokens.Dot(_, _)           => print "DOT\n"
    | Tokens.Else(_, _)          => print "ELSE\n"
    | Tokens.End(_, _)           => print "END\n"
    | Tokens.EndOfFile(_, _)     => print "ENDOFFILE\n"
    | Tokens.Exn(_, _)           => print "EXN\n"
    | Tokens.Function(_, _)      => print "FUNCTION\n"
    | Tokens.Handle(_, _)        => print "HANDLE\n"
    | Tokens.Identifier(_, _, v) => print ("IDENTIFIER(" ^ UniChar.Vector2String v ^ ")\n")
    | Tokens.If(_, _)            => print "IF\n"
    | Tokens.In(_, _)            => print "IN\n"
    | Tokens.Integer(_, _, v)    => print ("INTEGER(" ^ Int.toString v ^ ")\n")
    | Tokens.LBrace(_, _)        => print "LBRACE\n"
    | Tokens.LBrack(_, _)        => print "LBRACK\n"
    | Tokens.List(_, _)          => print "LIST\n"
    | Tokens.LParen(_, _)        => print "LPAREN\n"
    | Tokens.Mapsto(_, _)        => print "MAPSTO\n"
    | Tokens.Module(_, _)        => print "MODULE\n"
    | Tokens.Pipe(_, _)          => print "PIPE\n"
    | Tokens.Raise(_, _)         => print "RAISE\n"
    | Tokens.RBrace(_, _)        => print "RBRACE\n"
    | Tokens.RBrack(_, _)        => print "RBRACK\n"
    | Tokens.RParen(_, _)        => print "RPAREN\n"
    | Tokens.String(_, _, v)     => print "STRING()\n"
    | Tokens.Then(_, _)          => print "THEN\n"
    | Tokens.Type(_, _)          => print "TYPE\n"
    | Tokens.Val(_, _)           => print "VAL\n"

fun readFile filename = let
   fun doRead file = let
      val (tok, file') = Tokens.nextToken file
   in
      case tok of
         Tokens.EndOfFile(_, _) => OS.Process.exit 0
       | _  => ( printToken tok ; doRead file' )
   end
in
   doRead (openFile filename)
end
