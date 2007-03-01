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
   datatype Entry = SYM_EXN of {ty: Types.Type}
                  | SYM_FUNCTION of {ty: Types.Type, tyFormals: Symbol.symbol list,
                                     formals: (Symbol.symbol * Types.Type) list}
                  | SYM_MODULE
                  | SYM_TYCON of {ty: Types.Type, tyFormals: Symbol.symbol list}
                  | SYM_TYPE of {ty: Types.Type}
                  | SYM_VALUE of {ty: Types.Type}

   (* Thrown by the hash table internals. *)
   exception NotFound

   (* A symbol table is a polymorphic HashTable mapping Symbol.symbols to
    * entrys.
    *)
   type table = (Symbol.symbol, Entry) HashTable.hash_table

   (* Create a new empty symbol table. *)
   val mkTable: unit -> table

   (* Compare two symbols for equality. *)
   val symbolsEqual: Symbol.symbol * Symbol.symbol -> bool
end

structure Symtab :> SYMTAB = struct
   datatype Entry = SYM_EXN of {ty: Types.Type}
                  | SYM_FUNCTION of {ty: Types.Type, tyFormals: Symbol.symbol list,
                                     formals: (Symbol.symbol * Types.Type) list}
                  | SYM_MODULE
                  | SYM_TYCON of {ty: Types.Type, tyFormals: Symbol.symbol list}
                  | SYM_TYPE of {ty: Types.Type}
                  | SYM_VALUE of {ty: Types.Type}

   exception NotFound

   type table = (Symbol.symbol, Entry) HashTable.hash_table

   fun hashSymbol (sym:Symbol.symbol) = let
      (* Add a character to the front of the symbol to discriminate among
       * subtables.
       *)
      val discrim = case #2 sym of Symbol.EXN_TYPE => chr 1
                                 | Symbol.FUN_TYCON => chr 2
                                 | Symbol.MODULE => chr 3
                                 | Symbol.VALUE => chr 4
   in
      HashString.hashString (str discrim ^ MString.toString (#1 sym))
   end

   (* Two symbols are equal only if they are in the same subtable and if their
    * string representations are the same.
    *)
   fun symbolsEqual (a:Symbol.symbol, b:Symbol.symbol) =
      (#2 a = #2 b) andalso (MString.compare (#1 a, #1 b)) = EQUAL

   fun mkTable () =
      HashTable.mkTable (hashSymbol, symbolsEqual) (47, NotFound)
end
