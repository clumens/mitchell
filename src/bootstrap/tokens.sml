signature TOKENS =
sig
   datatype TokenKind = Absorb | Assign | Boolean of bool | Bottom | Case | Colon
                      | Comma | Dblquote | Decl | Dot | Else | End | EndOfFile | Exn
                      | Function | Handle | Identifier of UniChar.Data | If | In
                      | Integer of int | LBrace | LBrack | List | LParen | Mapsto
                      | Module | Pipe | Raise | RBrace | RBrack | RParen
                      | String of UniChar.Data | Then | Type | Union | Val

   type tokens = (int * int * TokenKind)

   val toString: tokens -> string
   val nextToken: Decode.DecFile -> tokens * Decode.DecFile
end

(* This structure defines the tokenizing portion of the mitchell compiler.
 * The entry point into this structure is nextToken, which takes a file object
 * and returns the next token read from the input file in the form of a
 * tokens object.  This structure should really only ever be needed by the
 * parser, though it's possible other structures may need to use the tokens
 * datatype.
 *)
structure Tokens :> TOKENS =
struct
   datatype TokenKind = Absorb | Assign | Boolean of bool | Bottom | Case | Colon
                      | Comma | Dblquote | Decl | Dot | Else | End | EndOfFile | Exn
                      | Function | Handle | Identifier of UniChar.Data | If | In
                      | Integer of int | LBrace | LBrack | List | LParen | Mapsto
                      | Module | Pipe | Raise | RBrace | RBrack | RParen
                      | String of UniChar.Data | Then | Type | Union | Val

   type tokens = (int * int * TokenKind)

   (* The current position in the input file. *)
   val lineno = ref 1
   val column = ref 0

   val reserved = [0wx22, 0wx23, 0wx28, 0wx29, 0wx2c, 0wx2e, 0wx3a, 0wx5b, 0wx5d, 0wx7b,
                   0wx7c, 0wx7d, 0wx0192, 0wx028b, 0wx03c4, 0wx2130, 0wx2133, 0wx2190,
                   0wx2192, 0wx222a, 0wx22a5]

   (* Convert a token datatype into a string representation. *)
   fun toString (_, _, Absorb) = "ABSORB"
     | toString (_, _, Assign) = "ASSIGN"
     | toString (_, _, Boolean v) = if v then "BOOLEAN(t)" else "BOOLEAN(f)"
     | toString (_, _, Bottom) = "BOTTOM"
     | toString (_, _, Case) = "CASE"
     | toString (_, _, Colon) = "COLON"
     | toString (_, _, Comma) = "COMMA"
     | toString (_, _, Dblquote) = "DBLQUOTE"
     | toString (_, _, Decl) = "DECL"
     | toString (_, _, Dot) = "DOT"
     | toString (_, _, Else) = "ELSE"
     | toString (_, _, End) = "END"
     | toString (_, _, EndOfFile) = "ENDOFFILE"
     | toString (_, _, Exn) = "EXN"
     | toString (_, _, Function) = "FUNCTION"
     | toString (_, _, Handle) = "HANDLE"
     | toString (_, _, Identifier i) = "IDENTIFIER()"
     | toString (_, _, If) = "IF"
     | toString (_, _, In) = "IN"
     | toString (_, _, Integer i) = "INTEGER(" ^ Int.toString(i) ^ ")"
     | toString (_, _, LBrace) = "LBRACE"
     | toString (_, _, LBrack) = "LBRACK"
     | toString (_, _, List) = "LIST"
     | toString (_, _, LParen) = "LPAREN"
     | toString (_, _, Mapsto) = "MAPSTO"
     | toString (_, _, Module) = "MODULE"
     | toString (_, _, Pipe) = "PIPE"
     | toString (_, _, Raise) = "RAISE"
     | toString (_, _, RBrace) = "RBRACE"
     | toString (_, _, RBrack) = "RBRACK"
     | toString (_, _, RParen) = "RPAREN"
     | toString (_, _, String s) = "STRING()"
     | toString (_, _, Then) = "THEN"
     | toString (_, _, Type) = "TYPE"
     | toString (_, _, Union) = "UNION"
     | toString (_, _, Val) = "VAL"

   (* Fetch the next token from the input file, returning a tokens * DecFile *)
   fun nextToken file = let
      fun isReserved ch =
         List.exists (fn ele => UniChar.compareChar (ele, ch) = EQUAL) reserved

      fun nextLine () = ( lineno := !lineno + 1 ; column := 0 ; () )
      fun nextCol () = ( column := !column + 1 ; () )
      fun prevCol () = ( column := !column - 1 ; () )

      (* raises: DecEof if eof is seen
       *         DecError for miscellaneous decoding errors
       * returns: UniChar.Char * DecFile
       *)
      fun readChar file = let
         val (ch, file') = Decode.decGetChar file
      in
         if ch = 0wxa then ( nextLine () ; (ch, file') )
         else ( nextCol () ; (ch, file') )
      end

      (* Skip through the rest of the line until we hit a newline.  If EOF is
       * reached, the exn will be propagated back up and handled by nextToken.
       *)
      fun skipComments file = let
         val (ch, file') = readChar file
      in
         if not (ch = 0wxa) then skipComments file'
         else file'
      end

      fun skipWhitespace file = let
         val (ch, file') = readChar file
      in
         if UniClasses.isS ch then skipWhitespace file'
         else ( prevCol () ; file )
      end

      (* Read a block of characters out of the file into a vector.  The end of
       * the block (or word - no, not the numeric type) is determined by the
       * isMember function.  If EOF is reached, the exn will be propagated back
       * up and handled by nextToken.
       *)
      fun readWord (lst, file, isMember) = let
         val (ch, file') = readChar file
      in
         if not (isMember ch) then ( prevCol () ; (rev lst, file) )
         else readWord (ch::lst, file', isMember)
      end

      (* Figure out what kind of word we've read.  The easy way is to convert
       * it into a string and see if it matches any of our reserved words.  If
       * not, must be some crazy new user-defined identifier.
       *)
      fun handleWord (lst, lineno, column) =
         case UniChar.Data2String lst of
            "absorb" => (lineno, column, Absorb)
          | "case"   => (lineno, column, Case)
          | "decl"   => (lineno, column, Decl)
          | "else"   => (lineno, column, Else)
          | "end"    => (lineno, column, End)
          | "f"      => (lineno, column, Boolean false)
          | "handle" => (lineno, column, Handle)
          | "if"     => (lineno, column, If)
          | "in"     => (lineno, column, In)
          | "list"   => (lineno, column, List)
          | "raise"  => (lineno, column, Raise)
          | "t"      => (lineno, column, Boolean true)
          | "then"   => (lineno, column, Then)
          | _        => (lineno, column, Identifier lst)

      fun handleInteger lst =
         Option.valOf (Int.fromString (UniChar.Data2String lst)) handle Option => raise Error.ParseError ("FIXME", !lineno, !column, "Unable to perform numeric conversion.")

      (* Tokenizing strings is the hardest part of this whole process because
       * of the escape sequences, multiple lines, etc.  Too bad they can't be
       * as simple as they were originally designed to be.
       *)
      fun readString (str, file) = let
         (* Converts the string sequence \uXXXX into a single unicode character. *)
         fun readEscapedUnicode (lst, file) = let
            (* Convert a four-element list into an integer, making sure that
             * elements are valid hex digits first.
             *)
            fun list2Int lst =
               if List.exists UniClasses.isHex lst then raise Error.ParseError ("FIXME", !lineno, !column, "Invalid escaped Unicode character sequence.")
               else StringCvt.scanString (Int.scan StringCvt.HEX) (UniChar.Data2String lst)
         in
            if List.length lst = 4 then
               case list2Int (rev lst) of
                  SOME i => (Word.fromInt i, file)
                | NONE   => raise Error.ParseError ("FIXME", !lineno, !column, "Invalid escaped Unicode character sequence.")
            else let
               val (ch, file') = readChar file handle DecEof => raise Error.ParseError ("FIXME", !lineno, !column, "Premature end of file while reading escaped Unicode character sequence.")
            in
               readEscapedUnicode (ch::lst, file')
            end
         end

         (* Convert escaped characters. *)
         fun convertEscaped (ch, file) =
            case ch of
               (0wxa|0wxd) => let                        (* line continuation *)
                                 val file' = skipWhitespace file handle DecEof => raise Error.ParseError ("FIXME", !lineno, !column, "Premature end of file while reading whitespace escape sequence.")
                                 val (ch', file'') = readChar file'
                              in
                                 if not (ch' = 0wx5c) then raise Error.ParseError ("FIXME", !lineno, !column, "String whitespace escape sequences must end with '\\'.")
                                 else (0wx0, file'')
                              end
             | 0wx6e => (0wxa, file)                     (* \n *)
             | 0wx74 => (0wx9, file)                     (* \t *)
             | 0wx75 => readEscapedUnicode ([], file)    (* \uXXXX *)
             | _     => (ch, file)

         val (ch, file') = readChar file
      in
         case ch of
            0wx22 => (rev str, file')  (* " *)
          | 0wx5c => let               (* \ *)
                        val (ch', file'') = readChar file' handle DecEof => raise Error.ParseError ("FIXME", !lineno, !column, "Premature end of file in string escape sequence")
                        val (unescaped, file''') = convertEscaped (ch', file'')
                     in
                        (* If there was a line continuation embedded in the
                         * string, unescaped is 0 and we should skip appending
                         * the null byte.
                         *)
                        if unescaped = 0wx0 then readString (str, file''')
                        else readString (unescaped::str, file''')
                     end
          | _     => readString (ch::str, file')
      end

      val (ch, file') = readChar (skipWhitespace file)
   in
      (* Having to use the numbers for these characters is unfortunate, but
       * welcome to Unicode in SML land.
       *)
      case ch of
         0wx2190 => ((!lineno, !column, Assign), file')
       | 0wx22a5 => ((!lineno, !column, Bottom), file')
       | 0wx3a   => ((!lineno, !column, Colon), file')
       | 0wx2c   => ((!lineno, !column, Comma), file')
       | 0wx23   => nextToken (skipComments file')
       | 0wx22   => let
                       val (startLine, startCol) = (!lineno, !column)
                       val (str, file'') = readString ([], file')
                    in
                       ((startLine, startCol, String str), file'')
                    end
       | 0wx2e   => ((!lineno, !column, Dot), file')
       | 0wx2130 => ((!lineno, !column, Exn), file')
       | 0wx0192 => ((!lineno, !column, Function), file')
       | (0wx30|0wx31|0wx32|0wx33|0wx34|0wx35|0wx36|0wx37|0wx38|0wx39) => let
                       val (startLine, startCol) = (!lineno, !column)
                       val (str, file'') = readWord ([ch], file', fn ch => UniClasses.isDec ch)
                    in
                       ((startLine, startCol, Integer (handleInteger str)), file'')
                    end
       | 0wx7b   => ((!lineno, !column, LBrace), file')
       | 0wx5b   => ((!lineno, !column, LBrack), file')
       | 0wx28   => ((!lineno, !column, LParen), file')
       | 0wx2192 => ((!lineno, !column, Mapsto), file')
       | 0wx2133 => ((!lineno, !column, Module), file')
       | 0wx7c   => ((!lineno, !column, Pipe), file')
       | 0wx7d   => ((!lineno, !column, RBrace), file')
       | 0wx5d   => ((!lineno, !column, RBrack), file')
       | 0wx29   => ((!lineno, !column, RParen), file')
       | 0wx03c4 => ((!lineno, !column, Type), file')
       | 0wx222a => ((!lineno, !column, Union), file')
       | 0wx028b => ((!lineno, !column, Val), file')
       | _       => let
                       val f = fn ch => not (UniClasses.isS ch) andalso not (isReserved ch)
                       val (startLine, startCol) = (!lineno, !column)
                       val (str, file'') = readWord ([ch], file', f)
                    in
                       (handleWord (str, startLine, startCol), file'')
                    end
   end handle DecEof => ((!lineno, !column, EndOfFile), file)
end
