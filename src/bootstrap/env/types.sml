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

   fun eq (tyA:Type, tyB:Type) = true

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
