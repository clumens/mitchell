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
