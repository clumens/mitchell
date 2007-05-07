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

(* This file defines symbols - the strings in mitchell programs that can be
 * entered into the symbol tables.  Symbols are the keys for the symbol tables,
 * and the stored values are defined in symtab.sml.
 *)
signature SYMBOL =
sig
   (* The symbol table is divided up into several subtables, which allows
    * different kinds of symbols to have the same name as long as we can figure
    * out from context which kind of symbol we're dealing with.  These are the
    * allowed subtables.  This would probably be better in semant.sml, but that
    * introduces circular references.
    *)
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE

   (* The key for symbol table operations. *)
   type symbol

   (* position * error message * problem symbol *)
   exception SymbolError of StreamPos.pos * string * symbol

   (* position * error message * problem identifier *)
   exception IdError of StreamPos.pos * string * MString.mstring list

   (* Are two symbols equal?  This must compare both the symbol names and the
    * subtable types.  Two symbols with the same name in different subtables
    * are not equal.
    *)
   val eq: symbol * symbol -> bool

   (* Format an IdError exception for printing. *)
   val idErrorToString: (StreamPos.pos * string * MString.mstring list) -> string

   (* Hash a symbol for entry/lookup into a symbol table. *)
   val hash: symbol -> Word.word

   (* Return the name associated with a symbol. *)
   val name: symbol -> MString.mstring

   (* Function to compare just symbol names.  This acts as String.>, but for
    * symbols.  This function basically exists just to use with
    * ListMisc.findDup.
    *)
   val nameGt: symbol * symbol -> bool

   (* Return the position of a symbol in the input stream. *)
   val pos: symbol -> StreamPos.pos

   (* Return the subtable a symbol exists in. *)
   val subtable: symbol -> Subtable

   (* Format a SymbolError exception for printing. *)
   val symbolErrorToString: (StreamPos.pos * string * symbol) -> string

   (* Perform the reverse operation - convert a symbol into a string that
    * is suitable for printing.
    *)
   val toString: symbol -> string

   (* Convert a string into a symbol that can be inserted into a symbol table.
    * The subtable is used to discriminate between the various kinds of symbols
    * that can all have the same name but live in the same table.
    *)
   val toSymbol: MString.mstring * Subtable * StreamPos.pos -> symbol
end
