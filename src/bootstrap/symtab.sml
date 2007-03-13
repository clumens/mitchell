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
   (* The structure stored in the hash table, keyed on Symbol.symbol. *)
   datatype Entry = SYM_EXN of Types.Type
                  | SYM_FUNCTION of {ty: Types.Type, tyFormals: Symbol.symbol list,
                                     formals: (Symbol.symbol * Types.Type) list}
                  | SYM_MODULE of table
                  | SYM_TYCON of {ty: Types.Type, tyFormals: Symbol.symbol list}
                  | SYM_TYPE of Types.Type
                  | SYM_VALUE of Types.Type

   (* A symbol table is a polymorphic HashTable mapping Symbol.symbols to
    * entrys.
    *)
   withtype table = (Symbol.symbol, Entry) HashTable.hash_table

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
   val insert: table -> Symbol.symbol * Entry -> unit
   val insert': table -> Symbol.symbol * Entry -> unit

   (* Two different ways to search a symbol table for an Entry.  The first
    * raises NotFound on error, the second returns NONE.
    *)
   val lookup: table -> Symbol.symbol -> Entry
   val find: table -> Symbol.symbol -> Entry option
end

structure Symtab :> SYMTAB = struct
   datatype Entry = SYM_EXN of Types.Type
                  | SYM_FUNCTION of {ty: Types.Type, tyFormals: Symbol.symbol list,
                                     formals: (Symbol.symbol * Types.Type) list}
                  | SYM_MODULE of table
                  | SYM_TYCON of {ty: Types.Type, tyFormals: Symbol.symbol list}
                  | SYM_TYPE of Types.Type
                  | SYM_VALUE of Types.Type

   withtype table = (Symbol.symbol, Entry) HashTable.hash_table

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
end

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
end
