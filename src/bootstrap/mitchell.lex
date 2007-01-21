%name MitchellLex;
%charset utf8;

%states COMMENTS STRINGS WSESCAPE;

%let digit = [0-9];
%let hex = [0-9A-Za-z];
%let id = [^{digit}].*;
%let int = {digit}+;
%let ws = ("\t"|"\n"|" ");

%defs (
   open MitchellParse.Tok

   val text: string list ref = ref []
   fun addText s = ( text := s::(!text) )
   fun clrText () = ( text := [] )
   fun getText () = concat (rev !text)
);

{ws}+ => ( continue() );

<INITIAL> "#"     => ( YYBEGIN COMMENTS );
<COMMENTS> "\n"   => ( YYBEGIN INITIAL );
<COMMENTS> .      => ( continue() );

<INITIAL> "\""          => ( clrText() ; YYBEGIN STRINGS );
<STRINGS> "\""          => ( YYBEGIN INITIAL ; STRING getText() );
<STRINGS> "\n"          => ( YYBEGIN WSESCAPE );
<STRINGS> "\\n"         => ( addText "\n" ; continue() );
<STRINGS> "\\t"         => ( addText "\t" ; continue() );
<STRINGS> "\\u"{hex}{4} => ( addText yytext() ; continue() );
<STRINGS> "\\".         => ( raise Errors.TokenizeError ("FIXME", yylineno, yypos, "Unknown escape sequence") );
<STRINGS> [^"\\]        => ( addText yytext() ; continue() );

<WSESCAPE> {ws}+  => ( continue() );
<WSESCAPE> "\\"   => ( YYBEGIN STRINGS );
<WSESCAPE> .      => ( raise Errors.TokenizeError ("FIXME", yylineno, yypos, "String whitespace escape sequences must end with '\\'.") );

<INITIAL> "absorb"   => ( ABSORB );
<INITIAL> "←"        => ( ASSIGN );
<INITIAL> "f"        => ( BOOLEAN false );
<INITIAL> "t"        => ( BOOLEAN true );
<INITIAL> "⊥"        => ( BOTTOM );
<INITIAL> "case"     => ( CASE );
<INITIAL> ":"        => ( COLON );
<INITIAL> ","        => ( COMMA );
<INITIAL> "decl"     => ( DECL );
<INITIAL> "."        => ( DOT );
<INITIAL> "else"     => ( ELSE );
<INITIAL> "end"      => ( END );
<INITIAL> "ℰ"        => ( EXN );
<INITIAL> "ƒ"        => ( FUNCTION );
<INITIAL> "handle"   => ( HANDLE );
<INITIAL> "if"       => ( IF );
<INITIAL> "in"       => ( IN );
<INITIAL> {int}      => ( let
                             val i = Int.fromString yytext()
                          in
                             case i of
                                SOME v => INTEGER v
                              | NONE   => raise Errors.TokenizeError ("FIXME", yylineno, yypos, "Unable to perform numeric conversion")
                          end );
<INITIAL> "{"        => ( LBRACE );
<INITIAL> "["        => ( LBRACK );
<INITIAL> "list"     => ( LIST );
<INITIAL> "("        => ( LPAREN );
<INITIAL> "→"        => ( MAPSTO );
<INITIAL> "ℳ"        => ( MODULE );
<INITIAL> "|"        => ( PIPE );
<INITIAL> "raise"    => ( RAISE );
<INITIAL> "}"        => ( RBRACE );
<INITIAL> "]"        => ( RBRACK );
<INITIAL> ")"        => ( RPAREN );
<INITIAL> ";"        => ( SEMICOLON );
<INITIAL> "then"     => ( THEN );
<INITIAL> "τ"        => ( TYPE );
<INITIAL> "∪"        => ( UNION );
<INITIAL> "ʋ"        => ( VAL ) ;
<INITIAL> {id}       => ( IDENTIFIER yyunicode() );
