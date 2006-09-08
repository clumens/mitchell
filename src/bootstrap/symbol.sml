(* This file defines the structures that make up symbol tables.  We are using
 * a hash table for each symbol table, and a stack of those tables to
 * represent lexical scope.  The topmost table on the stack represents the
 * current level of scope we're at, and will be the first to be examined.
 * This will also be the table where most new symbols will be added.  Leaving
 * a level of scope corresponds to removing this topmost table from the stack.
 *)
structure Symbol = struct
   datatype Subtable = EXN_TYPE | FUN_TYCON | MODULE | VALUE | NONE

   (* Actual symbol from the source file, mangled representation for
    * output, what kind of symbol it is.  The list is so we can have
    * dot-separated module element references.
    *)
   type symbol = (UniChar.Data * string) list * Subtable

   (* FIXME *)
   type symtab = bool

   (* FIXME - kill this *)
   fun empty () : symtab = false

   (* Convert a Mitchell identifier and given Subtable into a symbol,
    * mangling the unicode char list down into a string suitable for printing
    * out into an assembly file later on.
    *)
   val mangle = UniChar.Data2String

   fun toSymbol (unicodeSym, subtable) =
      ([(unicodeSym, mangle unicodeSym)], subtable)

   fun toString ((lst, _): symbol) =
      String.concatWith "." (map #2 lst)
end
