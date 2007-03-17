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
structure Entry :> ENTRY = struct
   datatype Entry = EXN of Types.Type
                  | FUNCTION of {ty: Types.Type, tyFormals: Symbol.symbol list,
                                 formals: (Symbol.symbol * Types.Type) list}
                  | MODULE
                  | TYCON of {ty: Types.Type, tyFormals: Symbol.symbol list}
                  | TYPE of Types.Type
                  | VALUE of Types.Type

   (* FIXME:  might be worth printing out tyFormals on FUNCTION and TYCON. *)
   fun toString (EXN ty) = Types.toString ty
     | toString (FUNCTION{ty, ...}) = Types.toString ty
     | toString MODULE = ""
     | toString (TYCON{ty, ...}) = Types.toString ty
     | toString (TYPE ty) = Types.toString ty
     | toString (VALUE ty) = Types.toString ty
end
