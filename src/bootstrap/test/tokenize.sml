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

(* Tests the tokenizing code by repeatedly fetching tokens and printing their
 * to stdout.
 *
 * Usage: readFile <filename>
 *)
fun readFile filename = let
   open MitchellTokens

   fun openFile filename =
      TextIO.openIn filename

   fun printToken tok = let
      val printFn = fn c => print (UTF8.toString c)
   in
      case tok of
         BOOLEAN false  => print "BOOLEAN(f)\n"
       | BOOLEAN true   => print "BOOLEAN(t)\n"
       | IDENTIFIER v   => ( print "IDENTIFIER(" ; app printFn v ; print ")\n" )
       | INTEGER v      => ( print ("INTEGER(" ^ Int.toString v ^ ")\n" ) )
       | STRING v       => ( print "STRING(\"" ; app printFn v ; print "\")\n" )
       | _              => print (toString tok ^ "\n")
   end

   val strm = openFile filename
   val sm = StreamPos.mkSourcemap()
   val lex = MitchellLex.lex sm

   fun doRead strm = let
      val (tok, pos, strm') = lex strm
   in
      printToken tok ; if isEOF tok then () else doRead strm'
   end
in
   doRead (MitchellLex.streamifyInstream strm)
   handle Error.TokenizeError e => print (filename ^ " " ^ (StreamPos.toString sm (#1 e)) ^
                                          ": " ^ #2 e ^ "\n")
end
