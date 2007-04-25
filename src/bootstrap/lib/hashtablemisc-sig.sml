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

(* Miscellaneous functions for handling hash tables. *)
signature HASHTABLEMISC = sig
   (* Given a hash table, a function to stringify the keys, and a function to
    * stringify the values, create a string representation of the whole table.
    *)
   val toString: ('a, 'b) HashTable.hash_table -> ('a -> string) -> ('b -> string) -> string
end
