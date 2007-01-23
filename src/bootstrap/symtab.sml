signature SYMTAB = sig
   (* The structure stored in the hash table, keyed on Symbol.symbol. *)
   datatype Entry = SYM_EXN of {ty: Types.Type}
                  | SYM_FUN of {ty: Types.Type, formals: (Symbol.symbol * Types.Type) list,
                                tyFormals: Symbol.symbol list}
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
end

structure Symtab :> SYMTAB = struct
   datatype Entry = SYM_EXN of {ty: Types.Type}
                  | SYM_FUNCTION of {ty: Types.Type, formals: (Symbol.symbol * Types.Type) list,
                                     tyFormals: Symbol.symbol list}
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
      UniChar.hashData ([UniChar.char2Char discrim] @ #1 sym)
   end

   fun compareSymbol (a:Symbol.symbol, b:Symbol.symbol) =
      (#2 a = #2 b) andalso UniChar.compareData (#1 a, #1 b) = EQUAL

   fun mkTable () =
      HashTable.mkTable (hashSymbol, compareSymbol) (47, NotFound)
end
