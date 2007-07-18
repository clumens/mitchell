(* mitchell - experimental compiler
 * Copyright (C) 2007 Chris Lumens
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
signature SEMANT =
sig
   (* error message *)
   exception InternalError of string

   (* position * error message * expected message * expected ty * got message * got ty *)
   exception TypeError of AntlrStreamPos.pos * string * string * Types.Type * string * Types.Type

   (* We only need to expose a single function for checking the entire AST. *)
   val checkProg: (string -> unit) -> Absyn.Decl list -> unit

   val typeErrorToString: (AntlrStreamPos.pos * string * string * Types.Type * string * Types.Type) -> string
end
