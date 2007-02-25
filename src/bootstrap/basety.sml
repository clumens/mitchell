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

(* This structure defines how certain types are represented inside the
 * compiler.  However, it does not define the types supported by the 
 * mitchell language.  For that information, look at types.sml.
 *)
structure BaseTy = struct
   (* The representation of a valid mitchell string (or identifier, etc.)
    * within the compiler.
    *)
   type mstring = UTF8.wchar list

   (* Convert a mitchell string into an ML string that can be printed. *)
   fun mstringToString str =
      String.concat (map UTF8.toString str)
end
