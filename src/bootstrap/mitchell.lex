%name MitchellLex;
%charset utf8;

%states COMMENTS STRINGS WSESCAPE;

%let digit = [0-9];
%let hex = [0-9A-Za-z];
%let id = [^{digit}].*;
%let int = {digit}+;
%let ws = [\t\n ];

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
<COMMENTS> "\n"   => ( YYBEGIN INITIAL ; continue() );
<COMMENTS> .      => ( continue() );

<INITIAL> "\""          => ( clrText() ; YYBEGIN STRINGS ; continue() );
<STRINGS> "\""          => ( YYBEGIN INITIAL ; STRING (getText()) );
<STRINGS> "\n"          => ( YYBEGIN WSESCAPE ; continue() );
<STRINGS> "\\n"         => ( addChar (UTF8.fromAscii #"\n") ; continue() );
<STRINGS> "\\t"         => ( addChar (UTF8.fromAscii #"\t") ; continue() );
<STRINGS> "\\u"{hex}{4} => ( addText yyunicode; continue() );
<STRINGS> "\\".         => ( raise Error.TokenizeError ("FIXME", yypos, "Unknown escape sequence") );
<STRINGS> [^"\\]        => ( addText yyunicode; continue() );

<WSESCAPE> " "|\n|\t => ( continue() );
<WSESCAPE> "\\"      => ( YYBEGIN STRINGS ; continue() );
<WSESCAPE> .         => ( raise Error.TokenizeError ("FIXME", yypos, "String whitespace escape sequences must end with '\\'.") );

<INITIAL> " "|\n|\t  => ( continue() );
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
                             val i = Int.fromString yytext
                          in
                             case i of
                                SOME v => INTEGER v
                              | NONE   => ( raise Error.TokenizeError ("FIXME", yypos, "Unable to perform numeric conversion") )
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
<INITIAL> {id}       => ( IDENTIFIER yyunicode );
