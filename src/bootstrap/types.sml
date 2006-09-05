(* This structure defines the representation of the basic types of the
 * mitchell language.
 *)
structure Types = struct
   (* Used for determining whether a type is part of an infinite loop. *)
   datatype Finite = NOT_FINITE | UNVISITED | VISITED | FINITE

   (* These are obvious except for a couple:
    *    ALIAS is when you make a new name for an existing type.
    *    ANY is the type of a RaiseExpr, since it needs to be equal to
    *        whatever type is expected for type checking purposes.
    *    BOTTOM is the type for when there's no other type.
    *    NONE_YET is what gets set in the AST while it's being built, but
    *             typechecking has not yet run.
    *)
   datatype Type = ALIAS of Symbol.symbol * Finite
                 | ANY of Finite
                 | BOOLEAN
                 | BOTTOM
                 | EXN of (Symbol.symbol * Type) list * Finite
                 | INTEGER
                 | LIST of Type * Finite
                 | NONE_YET
                 | RECORD of (Symbol.symbol * Type) list * Finite
                 | STRING
                 | UNION of (Symbol.symbol * Type option) list * Finite
end
