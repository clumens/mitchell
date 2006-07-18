(* Tests the tokenizing code by repeatedly fetching tokens and printing their
 * to stdout.
 *
 * Usage: readFile <filename>
 *)
CM.make ("../sources.cm");

fun openFile filename =
   Decode.decOpenUni (SOME(Uri.String2Uri filename), Encoding.UTF8)

fun posStr (tok:Tokens.tokens) = Int.toString (#1 tok) ^ ":" ^ Int.toString (#2 tok)

fun printToken (tok:Tokens.tokens) =
   case (#3 tok) of
      Tokens.Absorb        => print "ABSORB "
    | Tokens.Assign        => print "ASSIGN "
    | Tokens.Boolean(v)    => print ("BOOLEAN(" ^ (if v then "true) " else "false) "))
    | Tokens.Bottom        => print "BOTTOM "
    | Tokens.Case          => print "CASE "
    | Tokens.Colon         => print "COLON "
    | Tokens.Comma         => print "COMMA "
    | Tokens.Dblquote      => print "DBLQUOTE "
    | Tokens.Decl          => print "DECL "
    | Tokens.Dot           => print "DOT "
    | Tokens.Else          => print "ELSE "
    | Tokens.End           => print "END "
    | Tokens.EndOfFile     => print "ENDOFFILE "
    | Tokens.Exn           => print "EXN "
    | Tokens.Function      => print "FUNCTION "
    | Tokens.Handle        => print "HANDLE "
    | Tokens.Identifier(v) => print ("IDENTIFIER(" ^ UniChar.Data2String v ^ ") ")
    | Tokens.If            => print "IF "
    | Tokens.In            => print "IN "
    | Tokens.Integer(v)    => print ("INTEGER(" ^ Int.toString v ^ ") ")
    | Tokens.LBrace        => print "LBRACE "
    | Tokens.LBrack        => print "LBRACK "
    | Tokens.List          => print "LIST "
    | Tokens.LParen        => print "LPAREN "
    | Tokens.Mapsto        => print "MAPSTO "
    | Tokens.Module        => print "MODULE "
    | Tokens.Pipe          => print "PIPE "
    | Tokens.Raise         => print "RAISE "
    | Tokens.RBrace        => print "RBRACE "
    | Tokens.RBrack        => print "RBRACK "
    | Tokens.RParen        => print "RPAREN "
    | Tokens.String(v)     => print ("STRING(" ^ UniChar.Data2String v ^ ") ")
    | Tokens.Then          => print "THEN "
    | Tokens.Type          => print "TYPE "
    | Tokens.Union         => print "UNION "
    | Tokens.Val           => print "VAL "

fun readFile filename = let
   fun doRead file = let
      val (tok, file') = Tokens.nextToken file
   in
      case (#3 tok) of
         Tokens.EndOfFile => ()
       | _  => ( printToken tok ; print (posStr tok ^ "\n") ; doRead file' )
   end
in
   doRead (openFile filename)
end
