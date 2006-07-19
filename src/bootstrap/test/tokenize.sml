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
   print (Tokens.toString tok ^ " " ^ posStr tok ^ "\n")

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
