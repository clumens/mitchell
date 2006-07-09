(* This structure defines the representation of the basic types of the
 * mitchell language.
 *)
structure Types = struct
   (* Used for determining whether a type is part of an infinite loop. *)
   datatype Finite = NOT_FINITE | UNVISITED | VISITED | FINITE

   (* These are obvious except for a couple:
    *    TY_ALIAS is when you make a new name for an existing type.
    *    TY_ANY is the type of a RaiseExpr, since it needs to be equal to
    *           whatever type is expected for type checking purposes.
    *    TY_BOTTOM is the type for when there's no other type.
    *)
   datatype Type = ALIAS of Symbol.symbol * Finite
                 | ANY of Finite
                 | BOOLEAN
                 | BOTTOM of Finite
                 | EXN of (Symbol.symbol * Type) list * Finite
                 | INTEGER
                 | LIST of Type * Finite
                 | RECORD of (Symbol.symbol * Type) list * Finite
                 | STRING
end
