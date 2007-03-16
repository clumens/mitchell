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
signature SYMTAB = sig
   (* A symbol table is a mapping from symbols to entries. *)
   type table

   (* Raised by the hash table internals. *)
   exception NotFound

   (* Raised by insert if a symbol already exists. *)
   exception Duplicate

   (* Create a new empty symbol table. *)
   val mkTable: unit -> table

   (* Two different ways to add a new entry to the table.  The first raises
    * Duplicate if a symbol by that name already exists in the table.  The
    * second overrides any preexisting entry.
    *)
   val insert: table -> Symbol.symbol * Entry.Entry -> unit
   val insert': table -> Symbol.symbol * Entry.Entry -> unit

   (* Two different ways to search a symbol table for an Entry.  The first
    * raises NotFound on error, the second returns NONE.
    *)
   val lookup: table -> Symbol.symbol -> Entry.Entry
   val find: table -> Symbol.symbol -> Entry.Entry option

   (* Given a header and a table, return a string representation of the table's
    * contents.
    *)
   val toString: table -> string -> string
end

structure Symtab :> SYMTAB = struct
   type table = (Symbol.symbol, Entry.Entry) HashTable.hash_table

   exception NotFound

   exception Duplicate

   fun hashSymbol sym = let
      (* Add a character to the front of the symbol to discriminate among
       * subtables.
       *)
      val discrim = case Symbol.subtable sym of Symbol.EXN_TYPE => chr 1
                                              | Symbol.FUN_TYCON => chr 2
                                              | Symbol.MODULE => chr 3
                                              | Symbol.VALUE => chr 4
   in
      HashString.hashString (str discrim ^ MString.toString (Symbol.name sym))
   end

   fun mkTable () =
      HashTable.mkTable (hashSymbol, Symbol.eq) (47, NotFound)

   val insert' = HashTable.insert
   val lookup = HashTable.lookup
   val find = HashTable.find

   fun insert table (sym, entry) =
      case find table sym of SOME e => raise Duplicate
                           | NONE => insert' table (sym, entry)

   fun toString table hdr =
      "----------------------------------------\n" ^ hdr ^
      HashTable.foldi (fn (key, entry, v) => v ^ (Symbol.toString key) ^ " => " ^
                                                 (Entry.toString entry) ^ "\n")
                      "" table
end
