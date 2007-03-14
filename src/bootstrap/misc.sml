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
signature LISTMISC =
sig
   (* Given a comparison function and a list, return the first duplicate item
    * in the list.  The comparison function must return a boolean as must
    * act like the operator >.
    *)
   val findDup: ('a * 'a -> bool) -> 'a list -> 'a option

   (* Split a list into the head of the list that satisfies the predicate
    * function and all the elements from the first one that does not to the end.
    *)
   val split: ('a -> bool) -> 'a list -> 'a list * 'a list
end

structure ListMisc :> LISTMISC =
struct
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

   local
      fun doSplit f [] accum = (accum, [])
        | doSplit f (lst as x::xs) accum = if f x then doSplit f xs (x::accum)
                                           else (accum, lst)
   in
      fun split f lst = doSplit f lst []
   end
end
