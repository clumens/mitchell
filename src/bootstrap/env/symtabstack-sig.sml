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
signature SYMTAB_STACK = sig
   type stack

   (* Given a symbol table stack and a new table, push the new table onto the
    * top of the stack so all lookups and additions will be performed relative
    * to the new table.
    *)
   val enter: stack * Symtab.table -> stack

   (* Given a symbol table stack, pop the table off the top and return the new
    * stack and the table.  Raises Empty if the stack is empty.
    *)
   val leave: stack -> stack * Symtab.table

   (* Return the table at the top of the symbol table stack.  Raises Empty if
    * the stack is empty.
    *)
   val top: stack -> Symtab.table

   (* Two different ways to search a symbol table stack for an Entry.  The first
    * raises NotFound on error, the second returns NONE.
    *)
   val lookup: stack -> Symbol.symbol -> Entry.Entry
   val find: stack -> Symbol.symbol -> Entry.Entry option

   (* Given a header and a symbol table stack, return a string representation of
    * the complete current environment.
    *)
   val toString: stack -> string -> string
end
