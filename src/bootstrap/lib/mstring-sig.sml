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

(* How strings in the mitchell language are represented in the compiler.  This
 * does not define the string type of the mitchell language, however.
 *)
signature MSTRING = sig
   (* FIXME: I don't really like exposing the type in the signature, but it'll
    * do for now.
    *)
   type mstring = UTF8.wchar list

   (* Concatenate two mstrings. *)
   val ^ : mstring * mstring -> mstring

   (* These functions compare two mstrings, just like the ones in String. *)
   val compare: mstring * mstring -> order
   val < : mstring * mstring -> bool
   val <= : mstring * mstring -> bool
   val > : mstring * mstring -> bool
   val >= : mstring * mstring -> bool

   (* Convert a string into a mitchell string. *)
   val fromString: string -> mstring

   (* Convert a word into a string. *)
   val fromWord: Word.word -> mstring

   (* Convert a character into a mitchell string. *)
   val str: char -> mstring

   (* Convert a mitchell string to a native format string. *)
   val toString: mstring -> string
end
