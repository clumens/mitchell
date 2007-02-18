(* Tests the parsing code by running an input file through the parser and
 * printing any resulting errors.
 *
 * Usage: parseFile <filename>
 *)
structure Parser = MitchellParseFn(MitchellLex)

fun parseFile filename = let
   fun openFile filename =
      TextIO.openIn filename

   fun parse lex strm sm = let
      val (result, strm', errs) = Parser.parse lex strm
      (* filename [line.col]: Parse error: repair *)
      val errStrs = map (Repair.repairToString MitchellTokens.toString sm) errs
   in
      case result of
         SOME r => if length errStrs > 0 then
                      app (fn s => print (filename ^ " " ^ s ^ "\n")) errStrs
                   else
                      ()
       | NONE => print "unrecoverable error!\n"
   end

   val strm = openFile filename
   val sm = StreamPos.mkSourcemap()
   val lex = MitchellLex.lex sm
in
   parse lex (MitchellLex.streamifyInstream strm) sm
   handle Error.TokenizeError e => print (filename ^ " "^ (StreamPos.toString sm (#1 e)) ^
                                          ": " ^ #2 e ^ "\n")
end
