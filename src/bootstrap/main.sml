(* This is the entry point for the whole mitchell compiler.  This is where the
 * compilation process is controlled, and also where ml-build will know to
 * look to build a standalone executable.
 *)
structure Main =
struct
   open Error
   structure Parser = MitchellParseFn (MitchellLex)

   (* Given a filename as a string, return the abstract syntax tree.  This is
    * largely the same as test/parser.sml.
    *)
   fun parseFile filename = let
      (* Open the input file and return a TextIO.instream.  If the file cannot
       * be opened, print an error message and quit.
       *)
      fun openFile filename = 
         TextIO.openIn filename
         handle IO.Io e => ( print (filename ^ ": Error: Could not open file for reading.\n") ;
                             quit true )

      (* Print error messages from the parser.  This function is adapted
       * from repair.sml in the ml-lpt sources since we want differently
       * formatted output.
       *)
      fun repairToString tokToString sm (pos, repair) =
         ( filename ^ StreamPos.toString sm pos ^ ": Parse error: " ^
           Repair.actionToString tokToString repair
         )

      (* Perform the actual parse.  If there are any repair error messages,
       * print them out and then quit.  Otherwise return the AST.
       *)
      fun parse lexer strm sm = let
         val (result, strm', errs) = Parser.parse lexer strm
         val errStrs = map (repairToString MitchellTokens.toString sm) errs
      in
         if not (List.null errStrs) then ( app (fn s => print (s ^ "\n")) errStrs ; quit true )
         else result
      end

      val strm = openFile filename
      val sm = StreamPos.mkSourcemap ()
      val lexer = MitchellLex.lex sm

      val ast = parse lexer (MitchellLex.streamifyInstream strm) sm
                handle TokenizeError e => ( print (filename ^ " " ^
                                                  (StreamPos.toString sm (#1 e)) ^ ": " ^
                                                  #2 e ^ "\n") ;
                                            quit true )
   in
      (* What cases will cause parse to return NONE for the ast?  Perhaps we're
       * already handling those cases with TokenizeError and repairToString
       * above and don't need to worry about this.
       *)
      case ast of
         SOME lst => lst
       | NONE     => raise InternalError "Parser returned NONE for abstract syntax tree"
   end

   (* This is where the magic happens. *)
   fun main (name, argv) = let
      (* Wrapper around Absyn.write to handle specifying a destination. *)
      fun printAST ast inFile hdr (SOME (Options.AbsynFile (Options.Stdout))) =
             Absyn.write TextIO.stdOut hdr ast
        | printAST ast inFile hdr (SOME (Options.AbsynFile (Options.Default))) =
             Absyn.write (TextIO.openOut (inFile ^ ".ast")) hdr ast
        | printAST ast inFile hdr (SOME (Options.AbsynFile (Options.File f))) =
             Absyn.write (TextIO.openOut f) hdr ast
        | printAST ast inFile hdr _ = ()

      val (optsMap, extra) = Options.parse argv handle e => Options.badOpts e
      val inFile = hd extra handle Empty => Options.badOpts Options.NullArgExn

      val ast = parseFile inFile
      val _   = printAST ast inFile "Initial abstract syntax tree"
                         (StringMap.find (optsMap, "Idump-absyn"))
   in
      OS.Process.exit OS.Process.success
   end
end
