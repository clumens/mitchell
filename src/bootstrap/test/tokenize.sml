(* Tests the tokenizing code by repeatedly fetching tokens and printing their
 * to stdout.
 *
 * Usage: readFile <filename>
 *)

fun readFile filename = let
   fun openFile filename =
      TextIO.openIn filename

   fun printToken tok = print (MitchellTokens.toString tok ^ "\n")

   val strm = openFile filename
   val lex = MitchellLex.lex (StreamPos.mkSourcemap())

   fun doRead strm = let
      val (tok, pos, strm') = lex strm
   in
      printToken tok ; if MitchellTokens.isEOF tok then () else doRead strm'
   end
in
   doRead (MitchellLex.streamifyInstream strm)
end
