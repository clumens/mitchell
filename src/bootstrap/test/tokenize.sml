(* Tests the tokenizing code by repeatedly fetching tokens and printing their
 * to stdout.
 *
 * Usage: readFile <filename>
 *)

fun readFile filename = let
   open MitchellTokens

   fun openFile filename =
      TextIO.openIn filename

   fun printToken tok =
      case tok of
         IDENTIFIER str => ( print "IDENTIFIER(" ; app (fn c => print (UTF8.toString c))
                                                       str ;
                             print ")\n" )
       | _              => print (toString tok ^ "\n")

   val strm = openFile filename
   val lex = MitchellLex.lex (StreamPos.mkSourcemap())

   fun doRead strm = let
      val (tok, pos, strm') = lex strm
   in
      printToken tok ; if isEOF tok then () else doRead strm'
   end
in
   doRead (MitchellLex.streamifyInstream strm)
end
