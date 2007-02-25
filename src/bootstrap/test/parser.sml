(* mitchell - experimental compiler
 * Copyright (C) 2006, 2007 Chris Lumens
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

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
