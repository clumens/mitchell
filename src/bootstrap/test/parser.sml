(* Tests the parsing code by running an input file through the parser and
 * printing any resulting errors.
 *
 * Usage: parseFile <filename>
 *)
CM.make ("../sources.cm");

fun parseFile filename = let
   fun openFile filename =
      Decode.decOpenUni (SOME(Uri.String2Uri filename), Encoding.UTF8)

   fun printErr (file, line, col, msg) =
      print (file ^ ":" ^ Int.toString line ^ "." ^ Int.toString col ^
             " Error: Parse error on input file.\n\t" ^ msg ^ "\n")
in
   Parse.parse (openFile filename) handle Error.ParseError err => ( printErr err ; [] )
end
