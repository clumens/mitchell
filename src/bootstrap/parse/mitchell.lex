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
%name MitchellLex;
%charset utf8;

%states COMMENTS STRINGS WSESCAPE;

%let digit = [0-9];
%let hex = [0-9A-Fa-f];
%let int = {digit}+;
%let ws = \u0020|\n|\t;
%let id = [^0-9\u0020\n\t#"←⊥:,.ℰƒ{\u005b(→ℳ|}\u005d);τ∪ʋ][^\u0020\n\t#"←⊥:,.ℰƒ{\u005b(→ℳ|}\u005d);τ∪ʋ]*;

%defs (
   open MitchellTokens
   type lex_result = token
   fun eof() = EOF

   (* position * error message *)
   exception TokenizeError of AntlrStreamPos.pos * string

   val text: MString.mstring ref = ref (MString.fromString "")
   fun addText s = ( text := MString.^ (!text, s) )
   fun addChar ch = ( text := MString.^ (!text, MString.str ch) )
   fun addWord w = ( text := MString.^ (!text, MString.fromWord w) )
   fun clrText () = ( text := MString.fromString "" )
   fun getText () = !text

   fun convertEscaped esc pos =
      case Word.fromString (String.extract (esc, 2, NONE)) of
         SOME i => i
       | NONE   => raise TokenizeError (pos, "Invalid escaped Unicode character sequence.")
);

<INITIAL> "#"     => ( YYBEGIN COMMENTS ; continue() );
<COMMENTS> \n     => ( YYBEGIN INITIAL ; continue() );
<COMMENTS> .      => ( continue() );

<INITIAL> "\""          => ( clrText() ; YYBEGIN STRINGS ; continue() );
<STRINGS> "\""          => ( YYBEGIN INITIAL ; STRING (getText()) );
<STRINGS> "\\"\n        => ( YYBEGIN WSESCAPE ; continue() );
<STRINGS> "\\n"         => ( addChar #"\n" ; continue() );
<STRINGS> "\\t"         => ( addChar #"\t" ; continue() );
<STRINGS> "\\u"{hex}{4} => ( addWord (convertEscaped yytext yypos); continue() );
<STRINGS> .             => ( addText yyunicode; continue() );

<WSESCAPE> {ws}      => ( continue() );
<WSESCAPE> "\\"      => ( YYBEGIN STRINGS ; continue() );
<WSESCAPE> .         => ( raise TokenizeError (yypos, "String whitespace escape sequences must end with '\\'.") );

<INITIAL> absorb     => ( ABSORB );
<INITIAL> f          => ( BOOLEAN false );
<INITIAL> t          => ( BOOLEAN true );
<INITIAL> case       => ( CASE );
<INITIAL> decl       => ( DECL );
<INITIAL> else       => ( ELSE );
<INITIAL> end        => ( END );
<INITIAL> handle     => ( HANDLE );
<INITIAL> if         => ( IF );
<INITIAL> in         => ( IN );
<INITIAL> list       => ( LIST );
<INITIAL> raise      => ( RAISE );
<INITIAL> then       => ( THEN );

<INITIAL> "←"        => ( ASSIGN );
<INITIAL> "⊥"        => ( BOTTOM );
<INITIAL> ":"        => ( COLON );
<INITIAL> ","        => ( COMMA );
<INITIAL> "."        => ( DOT );
<INITIAL> "ℰ"        => ( EXN );
<INITIAL> "ƒ"        => ( FUNCTION );
<INITIAL> "{"        => ( LBRACE );
<INITIAL> "["        => ( LBRACK );
<INITIAL> "("        => ( LPAREN );
<INITIAL> "→"        => ( MAPSTO );
<INITIAL> "ℳ"        => ( MODULE );
<INITIAL> "|"        => ( PIPE );
<INITIAL> "}"        => ( RBRACE );
<INITIAL> "]"        => ( RBRACK );
<INITIAL> ")"        => ( RPAREN );
<INITIAL> ";"        => ( SEMICOLON );
<INITIAL> "τ"        => ( TYPE );
<INITIAL> "∪"        => ( UNION );
<INITIAL> "ʋ"        => ( VAL ) ;

<INITIAL> {ws}       => ( continue() );
<INITIAL> {int}      => ( let
                             val i = Int.fromString yytext
                          in
                             case i of
                                SOME v => INTEGER v
                              | NONE   => ( raise TokenizeError (yypos, "Unable to perform numeric conversion") )
                          end );
<INITIAL> {id}       => ( IDENTIFIER yyunicode );
<INITIAL> .          => ( raise TokenizeError (yypos, "Unknown character") );
