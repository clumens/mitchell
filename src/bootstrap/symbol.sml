(* This file defines symbols - the strings in mitchell programs that can be
 * entered into the symbol tables.  Symbols are the keys for the symbol tables,
 * and the stored values are defined in symtab.sml.
 *)
signature SYMBOL = sig
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE

   (* The key for symbol table operations. *)
   type symbol = BaseTy.mstring * Subtable

   (* Convert a string into a symbol that can be inserted into a symbol table.
    * The subtable is used to discriminate between the various kinds of symbols
    * that can all have the same name but live in the same table.
    *)
   val toSymbol: BaseTy.mstring * Subtable -> symbol

   (* Perform the reverse operation - convert a symbol into a string that
    * is suitable for printing.
    *)
   val toString: symbol -> string
end

structure Symbol :> SYMBOL = struct
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE

   type symbol = BaseTy.mstring * Subtable

   (* A wrapper around the symbol representation so callers don't have to know
    * about the internals.  The internal format could change in the future.
    *)
   fun toSymbol (unicodeSym, subtable) =
      (unicodeSym, subtable): symbol

   fun toString (sym: symbol) =
      (case #2 sym of EXN_TYPE => "EXN_TYPE: "
                    | FUN_TYCON => "FUN_TYCON: "
                    | MODULE => "MODULE: "
                    | VALUE => "VALUE: ") ^ BaseTy.mstringToString (#1 sym)
end
