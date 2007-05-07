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

(* This structure defines the internal representation of all supported types
 * in the mitchell language.  This includes representation in symbol tables
 * and in AST nodes.
 *)
signature TYPES =
sig
   (* Used for determining whether a type is part of an infinite loop. *)
   datatype Finite = NOT_FINITE | UNVISITED | VISITED | FINITE

   (* These are obvious except for a couple:
    *    ALIAS is when you make a new name for an existing type.
    *    ANY is the type of a RaiseExpr, since it needs to be equal to
    *        whatever type is expected for type checking purposes.
    *    BOTTOM is the type for when there's no other type.
    *)
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

   (* Compare two types for equality. *)
   val eq: Type * Type -> bool

   (* Convert a Type into a string suitable for printing. *)
   val toString: Type -> string
end
