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
structure Symbol :> SYMBOL =
struct
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE

   type symbol = MString.mstring * Label.label * Subtable

   exception SymbolError of symbol * string

   exception IdError of MString.mstring list * string

   fun eq (a: symbol, b: symbol) =
      (#3 a = #3 b) andalso (MString.compare (#1 a, #1 b)) = EQUAL

   (* These look stupid right now, but they won't if I decide to change
    * the internal format of a symbol sometime in the future.
    *)
   fun name (sym: symbol) =
      #1 sym

   fun nameGt (a, b) =
      MString.> (name a, name b)

   fun subtable (sym: symbol) =
      #3 sym

   fun toString sym = let
      val hdr = case subtable sym of EXN_TYPE => "EXN_TYPE{"
                                   | FUN_TYCON => "FUN_TYCON{"
                                   | MODULE => "MODULE{"
                                   | VALUE => "VALUE{"
   in
      hdr ^ MString.toString (name sym) ^ "," ^ Label.toString (#2 sym) ^ "}"
   end

   fun toSymbol (unicodeSym, subtable) =
      (unicodeSym, Label.toLabel unicodeSym, subtable)

   fun hash sym = let
      (* Add a character to the front of the symbol to discriminate among
       * subtables.
       *)
      val discrim = case subtable sym of EXN_TYPE => chr 1
                                       | FUN_TYCON => chr 2
                                       | MODULE => chr 3
                                       | VALUE => chr 4
   in
      HashString.hashString (str discrim ^ MString.toString (name sym))
   end
end
