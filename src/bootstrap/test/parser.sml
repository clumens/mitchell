(* Tests the parsing code by running an input file through the parser and
 * printing any resulting errors.
 *
 * Usage: parseFile <filename>
 *)
structure Parser = MitchellParseFn(MitchellLex)

fun parseFile filename = let
   fun openFile filename =
      TextIO.openIn filename

   (* Adapted from repair.sml in the ml-lpt sources. *)
   fun repairToString tokToString sm (pos, repair) = 
      (filename ^ StreamPos.toString sm pos ^ ": Parse error: " ^
       Repair.actionToString tokToString repair)

   fun parse lex strm sm = let
      val (result, strm', errs) = Parser.parse lex strm
      val errStrs = map (repairToString MitchellTokens.toString sm) errs
   in
      if length errStrs > 0 then app (fn s => print (s ^ "\n")) errStrs
      else ()
   end

   val strm = openFile filename
   val sm = StreamPos.mkSourcemap()
   val lex = MitchellLex.lex sm
in
   parse lex (MitchellLex.streamifyInstream strm) sm
   handle Error.TokenizeError e => print (filename ^ " "^ (StreamPos.toString sm (#1 e)) ^
                                          ": " ^ #2 e ^ "\n")
end
