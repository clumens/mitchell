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
structure Types :> TYPES =
struct
   datatype Finite = NOT_FINITE | UNVISITED | VISITED | FINITE

   datatype Type = ALIAS of Type * Finite
                 | ANY of Finite
                 | BOOLEAN
                 | BOTTOM
                 | EXN of (Symbol.symbol * Type) list * Finite
                 | INTEGER
                 | LIST of Type * Finite
                 | RECORD of (Symbol.symbol * Type) list * Finite
                 | STRING
                 | UNION of (Symbol.symbol * Type option) list * Finite

   fun unalias (ALIAS (ty, _)) = unalias ty
     | unalias ty = ty

   (* XXX: does this need modification for polymorphic types? *)
   local
      fun pairEq (tyCmp, (symA, tyA), (symB, tyB)) = Symbol.eq (symA, symB) andalso
                                                     tyCmp (tyA, tyB)

      fun listsEq (tyCmp, lstA, lstB) =
         ListPair.allEq (fn (a, b) => pairEq (tyCmp, a, b)) (lstA, lstB)
         handle ListPair.UnequalLengths => false

      fun optListsEq (tyCmp, lstA, lstB) =
         ListPair.allEq (fn ((symA, SOME tyA), (symB, SOME tyB)) => pairEq (tyCmp, (symA, tyA), (symB, tyB))
                          | ((symA, NONE), (symB, NONE)) => Symbol.eq (symA, symB)
                          | (_, _) => false)
                        (lstA, lstB)
         handle ListPair.UnequalLengths => false
   in
      fun eq (ALIAS (tyA, _), ALIAS (tyB, _)) = eq (unalias tyA, unalias tyB)
        | eq (ALIAS (tyA, _), tyB) = eq (unalias tyA, tyB)
        | eq (tyA, ALIAS (tyB, _)) = eq (tyA, unalias tyB)
        | eq (ANY _, _) = true
        | eq (_, ANY _) = true
        | eq (BOOLEAN, BOOLEAN) = true
        | eq (BOTTOM, BOTTOM) = true
        | eq (EXN (lstA, _), EXN (lstB, _)) = listsEq (eq, lstA, lstB)
        | eq (INTEGER, INTEGER) = true
        | eq (LIST (tyA, _), LIST (tyB, _)) = eq (tyA, tyB)
        | eq (RECORD (lstA, _), RECORD (lstB, _)) = listsEq (eq, lstA, lstB)
        | eq (STRING, STRING) = true
        | eq (UNION (lstA, _), UNION (lstB, _)) = optListsEq (eq, lstA, lstB)
        | eq (_, _) = false
   end

   fun toString (ALIAS (ty, _)) = toString ty
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
