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

(* This structure defines a symbol table - a table mapping symbols found in
 * the abstract syntax tree to its definition (this mainly includes type
 * information).  Each new level of scope (modules, decl-exprs, functions,
 * etc.) creates a new symbol table.
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
