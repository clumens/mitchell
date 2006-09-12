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
    *)
   datatype Type = ALIAS of Symbol.symbol * Finite
                 | ANY of Finite
                 | BOOLEAN
                 | BOTTOM
                 | EXN of (Symbol.symbol * Type) list * Finite
                 | INTEGER
                 | LIST of Type * Finite
                 | RECORD of (Symbol.symbol * Type) list * Finite
                 | STRING
                 | UNION of (Symbol.symbol * Type option) list * Finite

   fun toString (ALIAS (sym, _)) = Symbol.toString sym
     | toString (ANY _) = "any"
     | toString BOOLEAN = "boolean"
     | toString BOTTOM = "bottom"
     | toString (EXN (lst, _)) = 
          "exn { " ^
          String.concatWith ", " (map (fn (sym, ty) => Symbol.toString sym ^ ": " ^
                                                       toString ty) lst) ^
          "}"
     | toString INTEGER = "integer"
     | toString (LIST (ty, _)) = "list " ^ toString ty
     | toString (RECORD (lst, _)) =
          "record { " ^
          String.concatWith ", " (map (fn (sym, ty) => Symbol.toString sym ^ ": " ^
                                                       toString ty) lst) ^
          "}"
     | toString STRING = "string"
     | toString (UNION (lst, _)) = 
          "union { " ^
          String.concatWith ", " (map (fn (sym, ty) => Symbol.toString sym ^
                                                       (case ty of SOME t => ": " ^ toString t
                                                                 | NONE => "")) lst) ^
          "}"
end
