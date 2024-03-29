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

(* This structure contains the datatypes and code to process command line
 * options that may be supplied to the compiler.  It makes use of David
 * Shea's improved getopt library, found in getopt/.
 *)
structure Options = struct
   val version = "mitchell version 20070426"
   val copyright = "© 2004-2007 Chris Lumens"

   (* Raised if invalid arguments provided *)
   exception ArgumentExn of string

   (* Raised if no arguments are provided. *)
   exception NullArgExn

   (* Specifies the destination for options that can write to a file.  Self
    * explanatory, except for Default which is the default file given by the
    * help output for the option.  In general, this is of the form
    * inputfile.ext.
    *)
   datatype output_file = Stdout
                        | Default of string
                        | File of string

   (* Specify which compilation phase should be the last one. *)
   datatype last_phase = Parser

   (* Control warnings from the compiler. *)
   datatype warnings = Werror

   (* The main datatype for option processing. *)
   datatype option_result = AbsynFile of output_file
                          | FreeValsFile of output_file
                          | LastPhase of last_phase
                          | PrintHelp
                          | PrintVersion
                          | SymtabFile of output_file
                          | Warning of warnings

   (* All the options and the code to process them gets shoved into this one
    * very complex value full of lambdas and stuff.
    *)
   val options = [
      (* general options *)
                   {short="v",
                    long=["version"],
                    help="Print compiler version and copyright information",
                    desc=GetOpt.NoArg (fn () => PrintVersion)
                   },
                   {short="h",
                    long=["help"],
                    help="Print options",
                    desc=GetOpt.NoArg (fn () => PrintHelp)
                   },

      (* warning options *)
                   {short="",
                    long=["Werror"],
                    help="Stop compilation on warnings, as on errors.",
                    desc=GetOpt.NoArg (fn () => Warning Werror)
                   },

      (* options affecting compliation phases *)
                   {short="",
                    long=["last-phase"],
                    (* previously allowed: typecheck, desugar-case,
                     * desugar-decl, lambda-lift
                     *)
                    help="Stop compilation after the given phase.  See the man page for details.  Possible values are: parser",
                    desc=GetOpt.ReqArg ((fn "parser" => LastPhase Parser
                                          | _ => raise ArgumentExn "Invalid option supplied to --last-phase"
                                        ), "PHASE")
                   },

      (* internals options *)
                   {short="",
                    long=["Idump-absyn"],
                    help="Dump the abstract syntax tree to FILE, or infile.ast by default",
                    desc=GetOpt.OptArg ((fn SOME "-" => AbsynFile Stdout
                                          | SOME f   => AbsynFile (File f)
                                          | NONE     => AbsynFile (Default ".ast")
                                        ), "FILE")
                   },
                   {short="",
                    long=["Idump-symtab"],
                    help="Dump the symbol tables to FILE, or infile.symtab by default",
                    desc=GetOpt.OptArg ((fn SOME "-" => SymtabFile Stdout
                                          | SOME f   => SymtabFile (File f)
                                          | NONE     => SymtabFile (Default ".symtab")
                                        ), "FILE")
                   },
                   {short="",
                    long=["Idump-free-vals"],
                    help="Dump the free values to FILE, or infile.free by default",
                    desc=GetOpt.OptArg ((fn SOME "-" => FreeValsFile Stdout
                                          | SOME f   => FreeValsFile (File f)
                                          | NONE     => FreeValsFile (Default ".free")
                                        ), "FILE")
                   }
                 ]

   (* Prints the help output to the screen and quit. *)
   fun printHelp () =
      ( print ( GetOpt.usageInfo {header="mitchell <infile> [OPTIONS]\n" ^
                                        "Compiler for the mitchell programming language.\n",
                                 options=options}) ;
       Error.quit false )

   (* Print the version information and quit. *)
   fun printVersion () =
      ( print (version ^ "\n" ^ copyright ^ "\n") ; Error.quit false )

   (* Handle various error cases that can occur in option processing and
    * quit.  This is only split out from option parsing so that the called can
    * see the exceptions and do something else if they want.
    *)
   fun badOpts (ArgumentExn opt) = ( print ("Invalid option: " ^ opt ^ "\n") ;
                                     printHelp () ;
                                     Error.quit true )

     | badOpts NullArgExn = ( printHelp () ; Error.quit true )
     | badOpts e = raise e

   (* Returns a list of argument results and a list of extra arguments.  Input
    * file should be in the extra list.
    *)
   fun parse argv =
      if List.null argv then raise NullArgExn
      else let val (optsMap, extra) = GetOpt.getOpt {argOrder=GetOpt.Permute,
                                                     errFn=(fn opt => raise ArgumentExn opt),
                                                     options=options} argv

               (* Handle a couple options right away. *)
               val _ = case StringMap.find (optsMap, "help") of
                          SOME _ => printHelp ()
                        | NONE   => ()
               val _ = case StringMap.find (optsMap, "version") of
                          SOME _ => printVersion ()
                        | NONE   => ()
           in
              (optsMap, extra)
           end
end
