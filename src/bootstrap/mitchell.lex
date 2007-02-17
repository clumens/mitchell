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

   val text: BaseTy.mstring ref = ref []
   fun addText s = ( text := List.revAppend (s, !text))
   fun addChar ch = ( text := ch::(!text))
   fun clrText () = ( text := [] )
   fun getText () = rev (!text)
);

<INITIAL> "#"     => ( YYBEGIN COMMENTS ; continue() );
<COMMENTS> \n     => ( YYBEGIN INITIAL ; continue() );
<COMMENTS> .      => ( continue() );

(*
<INITIAL> "\""          => ( clrText() ; YYBEGIN STRINGS ; continue() );
<STRINGS> "\""          => ( YYBEGIN INITIAL ; STRING (getText()) );
<STRINGS> \n            => ( YYBEGIN WSESCAPE ; continue() );
<STRINGS> "\\n"         => ( addChar (UTF8.fromAscii #"\n") ; continue() );
<STRINGS> "\\t"         => ( addChar (UTF8.fromAscii #"\t") ; continue() );
<STRINGS> "\\u"{hex}{4} => ( addText yyunicode; continue() );
<STRINGS> "\\".         => ( raise Error.TokenizeError ("FIXME", yypos, "Unknown escape sequence") );
<STRINGS> [^"\\]        => ( addText yyunicode; continue() );

<WSESCAPE> {ws}      => ( continue() );
<WSESCAPE> "\\"      => ( YYBEGIN STRINGS ; continue() );
<WSESCAPE> .         => ( raise Error.TokenizeError ("FIXME", yypos, "String whitespace escape sequences must end with '\\'.") );
*)

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
                              | NONE   => ( raise Error.TokenizeError ("FIXME", yypos, "Unable to perform numeric conversion") )
                          end );
<INITIAL> {id}       => ( IDENTIFIER yyunicode );
<INITIAL> .          => ( raise Error.TokenizeError ("FIXME", yypos, "Unknown character") );
