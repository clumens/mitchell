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

(* This functor constructs a stack of symbol tables.  It accepts a single
 * structure argument that matches the MONO_HASH_TABLE signature, though we
 * aren't so demanding right here.  This is intended to make stacks for
 * symbol tables and module tables.
 *)
functor TableStackFn (Table: sig
                         type 'a hash_table
                         val lookup: 'a hash_table -> Symbol.symbol -> 'a
                      end) :> TABLESTACK where type 'a table = 'a Table.hash_table = struct
   exception NotFound

   type 'a table = 'a Table.hash_table
   type 'a stack = 'a table list

   fun mkStack () = []

   fun enter (ts, tbl) = tbl :: ts

   fun leave ts = case List.getItem ts of
                     NONE => raise Empty
                   | SOME (tbl, ts') => (ts', tbl)

   fun top ts = hd ts

   local
      fun doLookup [] sym = raise NotFound
        | doLookup (tbl::ts) sym = Table.lookup tbl sym
                                   handle NotFound => doLookup ts sym
   in
      fun lookup ts sym = doLookup ts sym
      fun find ts sym = SOME(lookup ts sym) handle NotFound => NONE
   end
end
