(* mitchell - experimental compiler
 * Copyright (C) 2007 Chris Lumens
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

(* XXX: This is all temporary code.  It used to be in semant.sml, but there's no
 * reason for that file to have so much temporary crud in it.  Later, this will
 * be replaced by a proper module loader.  However that is a very long way off.
 *)
structure TempEnv :> sig
                        val createEnv: unit -> Entry.Entry SymtabStack.stack * 'a ModuletabStack.stack
                     end = struct
   (* Wrappers to create symbols. *)
   local
      fun mkSym str ty = Symbol.toSymbol (MString.fromString str, ty, 0)
   in
      fun funSym str = mkSym str Symbol.FUN_TYCON
      fun typeSym str = mkSym str Symbol.EXN_TYPE
      fun valueSym str = mkSym str Symbol.VALUE
   end

   (* This creates the base environment, containing all the predefined values,
    * functions, modules, and types.  Keep this as small as possible.  It might
    * be a better idea to have this automatically loaded from some other file,
    * like standard library stuff would be.
    *)
   fun mkBaseEnv () = let
      val globalSymtab = Symtab.mkTable (47, SymtabStack.NotFound)

      val syms = [ (valueSym "f", Entry.VALUE Types.BOOLEAN),
                   (valueSym "t", Entry.VALUE Types.BOOLEAN),
                   (valueSym "⊥", Entry.VALUE Types.BOTTOM),
                   (typeSym "boolean", Entry.TYPE Types.BOOLEAN),
                   (typeSym "integer", Entry.TYPE Types.INTEGER),
                   (typeSym "string", Entry.TYPE Types.STRING)
                 ]
   in
      ( app (Symtab.insert globalSymtab) syms ; globalSymtab )
   end

   fun mkIntegerEnv () = let
      val integerSymtab = Symtab.mkTable (47, SymtabStack.NotFound)

      val syms = [ (funSym "+", Entry.FUNCTION {ty=Types.INTEGER, formals=[(valueSym "x", Types.INTEGER), (valueSym "y", Types.INTEGER)], tyFormals=[]}),
                   (funSym "-", Entry.FUNCTION {ty=Types.INTEGER, formals=[(valueSym "x", Types.INTEGER), (valueSym "y", Types.INTEGER)], tyFormals=[]}),
                   (funSym "*", Entry.FUNCTION {ty=Types.INTEGER, formals=[(valueSym "x", Types.INTEGER), (valueSym "y", Types.INTEGER)], tyFormals=[]}),
                   (funSym "mod", Entry.FUNCTION {ty=Types.INTEGER, formals=[(valueSym "x", Types.INTEGER), (valueSym "y", Types.INTEGER)], tyFormals=[]}),
                   (funSym "<", Entry.FUNCTION {ty=Types.BOOLEAN, formals=[(valueSym "x", Types.INTEGER), (valueSym "y", Types.INTEGER)], tyFormals=[]}),
                   (funSym "=", Entry.FUNCTION {ty=Types.BOOLEAN, formals=[(valueSym "x", Types.INTEGER), (valueSym "y", Types.INTEGER)], tyFormals=[]})
                 ]
   in
      ( (app (Symtab.insert integerSymtab) syms) ; integerSymtab )
   end

   fun mkBooleanEnv () = let
      val booleanSymtab = Symtab.mkTable (47, SymtabStack.NotFound)

      val syms = [ (funSym "or", Entry.FUNCTION {ty=Types.BOOLEAN, formals=[(valueSym "x", Types.BOOLEAN), (valueSym "y", Types.BOOLEAN)], tyFormals=[]}) ]
   in
      ( (app (Symtab.insert booleanSymtab) syms) ; booleanSymtab )
   end

   fun createEnv () = let
      (* Create the global symbol table and module environment. *)
      val globalSymtab = mkBaseEnv ()
      val globalModuletab = Moduletab.mkTable (47, ModuletabStack.NotFound)

      (* Create symbols and symtabs for the Integer and Boolean modules.
       * Later, these will be loaded in automatically.
       *)
      val integerSymtab = mkIntegerEnv ()
      val booleanSymtab = mkBooleanEnv ()
      val integerSym = Symbol.toSymbol (MString.fromString "Integer", Symbol.MODULE, 0)
      val booleanSym = Symbol.toSymbol (MString.fromString "Boolean", Symbol.MODULE, 0)

      (* Add the symbols to the global environments. *)
      val _ = Symtab.insert globalSymtab (integerSym, Entry.MODULE)
      val _ = Symtab.insert globalSymtab (booleanSym, Entry.MODULE)
(*
      val _ = Moduletab.insert globalModuletab (integerSym, integerSymtab)
      val _ = Moduletab.insert globalModuletab (booleanSym, booleanSymtab)
*)

      (* Put the new tables onto the environment stacks. *)
      val ts = SymtabStack.enter (SymtabStack.mkStack (), globalSymtab)
      val ms = ModuletabStack.enter (ModuletabStack.mkStack (), globalModuletab)
   in
      (ts, ms)
   end
end
