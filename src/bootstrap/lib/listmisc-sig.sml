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
