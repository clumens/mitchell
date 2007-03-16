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
structure MString :> MSTRING = struct
   type mstring = UTF8.wchar list

   fun op ^ (strA, strB) =
      strA @ strB

   fun fromString s =
      map UTF8.fromAscii (String.explode s)

   fun fromWord (w: Word32.word) =
      [w]

   fun str ch =
      [UTF8.fromAscii ch]

   fun toString s =
      String.concat (map UTF8.toString s)

   (* Utility function for use in equality-testing functions below. *)
   fun cmp f ([], []) = EQUAL
     | cmp f (strA, []) = GREATER
     | cmp f ([], strB) = LESS
     | cmp f (chA::strA, chB::strB) = let
          val result = f (chA, chB)
       in
          if result = EQUAL then cmp f (strA, strB)
          else result
       end

   fun compare (strA, strB) = cmp Word32.compare (strA, strB)

   fun op < (strA, strB) = cmp Word32.compare (strA, strB) = LESS
   fun op <= (strA, strB) = not (cmp Word32.compare (strA, strB) = GREATER)
   fun op > (strA, strB) = cmp Word32.compare (strA, strB) = GREATER
   fun op >= (strA, strB) = not (cmp Word32.compare (strA, strB) = LESS)
end
