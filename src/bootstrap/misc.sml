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

(* Miscellaneous functions for handling lists of things. *)
structure ListMisc =
struct
   (* Given a list and a comparison function (for sorting), return the first
    * duplicate item.  The comparison function must return a boolean (not an
    * order, sadly).  The function returns an option with the duplicate item.
    *)
   fun findDup (op >) lst = let
      fun areEq (ele::next::_) = ele > next
        | areEq _ = false

      fun loop (ele::lst) = if areEq (ele::lst) then SOME(ele)
                            else loop lst
        | loop _ = NONE

      (* Sorting the list first makes everything else work. *)
      val lst' = if ListMergeSort.sorted (op >) lst then lst
                 else ListMergeSort.sort (op >) lst
   in
      loop lst'
   end
end
