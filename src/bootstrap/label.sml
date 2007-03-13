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

(* This file defines labels - the actual text that gets emitted for mitchell
 * symbols in the output code, stripped of its Unicode characters and turned
 * into unique identifiers.
 *)
signature LABEL = sig
   type label

   (* Convert an identifier represented as a mitchell string into a label. *)
   val toLabel: MString.mstring -> label
end

structure Label :> LABEL = struct
   type label = string

   val uniqueId = ref 0

   fun toLabel mstring =
      ("_." ^ Int.toString (!uniqueId) ^ "_" ^ MString.toString mstring) before
      uniqueId := !uniqueId + 1
end
