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

(* This signature defines a stack of symbol tables that can be used in the
 * semantic analysis phase.  This is what makes lexical scope work.
 *)
signature TABLESTACK = sig
   exception NotFound

   type 'a table
   type 'a stack

   (* Create a new empty stack. *)
   val mkStack: unit -> 'a stack

   (* Given a table stack and a new table, push the new table onto the top
    * of the stack so all lookups and additions will be performed relative to
    * this new table.
    *)
   val enter: 'a stack * 'a table -> 'a stack

   (* Given a table stack, pop the table off the top and return the stack and
    * the table.  Raises Empty if the stack is empty.
    *)
   val leave: 'a stack -> 'a stack * 'a table

   (* Return the table at the top of the table stack.  Raises Empty if the stack
    * is empty.
    *)
   val top: 'a stack -> 'a table

   (* Return the table at the bottom of the table stack.  This should correspond
    * to the global environment.  Raises Empty if the stack is empty.
    *)
   val bottom: 'a stack -> 'a table

   (* Two different ways to search a table stack for a Symbol.  The first raises
    * NotFound on error, the second returns NONE.
    *)
   val lookup: 'a stack -> Symbol.symbol -> 'a
   val find: 'a stack -> Symbol.symbol -> 'a option
end
