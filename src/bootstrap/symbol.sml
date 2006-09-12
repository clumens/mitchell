(* This file defines the data structures and operations required for symbol
 * table manipulation.  This consists of symbols and symbol tables.
 *)
signature SYMBOL = sig
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE

   (* The key for symbol table operations. *)
   type symbol = BaseTy.mstring * Subtable

   val toSymbol: BaseTy.mstring * Subtable -> symbol
   val toString: symbol -> string
end

structure Symbol :> SYMBOL = struct
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE

   type symbol = BaseTy.mstring * Subtable

   (* A wrapper around the symbol representation so callers don't have to know
    * about the internals.
    *)
   fun toSymbol (unicodeSym, subtable) =
      (unicodeSym, subtable): symbol

   fun toString (sym: symbol) =
      (case #2 sym of EXN_TYPE => "EXN_TYPE: "
                    | FUN_TYCON => "FUN_TYCON: "
                    | MODULE => "MODULE: "
                    | VALUE => "VALUE: ") ^ BaseTy.mstringToString (#1 sym)
end

signature SYMTAB = sig
   (* The structure stored in the hash table, keyed on Symbol.symbol. *)
   type entry = bool

   (* Thrown by the hash table internals. *)
   exception NotFound

   (* A symbol table is a polymorphic HashTable mapping Symbol.symbols to
    * entrys.
    *)
   type table = (Symbol.symbol, entry) HashTable.hash_table

   (* Create a new empty symbol table. *)
   val mkTable: unit -> table
end

structure Symtab :> SYMTAB = struct
   type entry = bool

   exception NotFound

   type table = (Symbol.symbol, entry) HashTable.hash_table

   fun hashSymbol (sym:Symbol.symbol) = let
      (* Add a character to the front of the symbol to discriminate among
       * subtables.
       *)
      val discrim = case #2 sym of Symbol.EXN_TYPE => chr 1
                                 | Symbol.FUN_TYCON => chr 2
                                 | Symbol.MODULE => chr 3
                                 | Symbol.VALUE => chr 4
   in
      UniChar.hashData ([UniChar.char2Char discrim] @ #1 sym)
   end

   fun compareSymbol (a:Symbol.symbol, b:Symbol.symbol) =
      (#2 a = #2 b) andalso UniChar.compareData (#1 a, #1 b) = EQUAL

   fun mkTable () =
      HashTable.mkTable (hashSymbol, compareSymbol) (47, NotFound)
end
