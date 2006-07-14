(* This file defines the structures that make up symbol tables.  We are using
 * a hash table for each symbol table, and a stack of those tables to
 * represent lexical scope.  The topmost table on the stack represents the
 * current level of scope we're at, and will be the first to be examined.
 * This will also be the table where most new symbols will be added.  Leaving
 * a level of scope corresponds to removing this topmost table from the stack.
 *)
structure Symbol = struct
   (* FIXME:  This comment and the whole Subtable type may die when we get
    * around to implementing the symbol tables.  Don't know yet.
    *)

   (* Our big symbol table actually contains three different name spaces.
    * This datatype is how we tell them apart, and this value will get added
    * in with the symbol name in the hash function so three symbols with the
    * same name can still coexist.  Note: FUNCTION and VALUE are different
    * tycons, but must be handled the same by the hashing function.  Same
    * with EXN and TYPE.  This is to prevent problems telling them apart in
    * parsing.
    *
    * We are overloading the use of this datatype.  It is also used by the
    * AST to describe exactly how a sybol is used.  Therefore, we also need
    * a NONE for things like record element references that don't go in
    * any subtable.
    *)
   datatype Subtable = EXN | FUNCTION | MODULE | TYPE | VALUE | NONE

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
   fun toSymbol (unicodeSym, subtable) =
      ([(unicodeSym, UniChar.Data2String unicodeSym)], subtable)
end
