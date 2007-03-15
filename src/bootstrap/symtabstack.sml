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
   val lookup: stack -> Symbol.symbol -> Symtab.Entry
   val find: stack -> Symbol.symbol -> Symtab.Entry option

   (* Given a header and a symbol table stack, return a string representation of
    * the complete current environment.
    *)
   val toString: stack -> string -> string
end

structure SymtabStack :> SYMTAB_STACK = struct
   type stack = Symtab.table list

   fun enter (ts, table) = table :: ts

   fun leave ts = case List.getItem ts of
                     NONE => raise Empty
                   | SOME (table, ts') => (ts', table)

   (* XXX: symtab_add_entry from c-mitchell should be implemented by calling
    * Symtab.insert on the result of SymtabStack.top.
    *)
   fun top ts = hd ts

   local
      fun doLookup [] sym = raise Empty
        | doLookup (table::ts) sym = Symtab.lookup table sym
                                     handle NotFound => doLookup ts sym
   in
      fun lookup ts sym = doLookup ts sym
      fun find ts sym = SOME(lookup ts sym) handle NotFound => NONE
   end

   fun toString ts hdr =
      "========================================\n" ^ hdr ^
      foldl (fn (table, v) => v ^ Symtab.toString table hdr) "" ts
end
