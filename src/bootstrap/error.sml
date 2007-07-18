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
structure Error = struct
   (* error message *)
   exception InternalError of string

   (* Format error messages to all look the same. *)
   fun fmtError (filename, sm, pos, msg) =
      filename ^ " " ^ (AntlrStreamPos.toString sm pos) ^ ": " ^ msg ^ "\n"

   (* A function to kill the compiler.  failure is a boolean for whether this
    * is an error case or not.  This function mainly exists to easily turn off
    * during development when we don't want to kill sml.
    *)
   fun quit failure = raise (Fail "mitchell encountered an error, dropping back to sml")
(*
   fun quit failure = if failure then OS.Process.exit OS.Process.failure
                      else OS.Process.exit OS.Process.success
*)
end
