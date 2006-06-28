structure Tokens =
struct
   exception UnknownToken of UniChar.Char

   (* All the types of valid Mitchell tokens.  The first two ints are the
    * lineno and column where the token occurs (or starts, in the case of really
    * long things).  Any additional parameters are easy to figure out.
    *)
   datatype tokens = Assign of int * int
                   | Boolean of int * int * bool
                   | Bottom of int * int
                   | Case of int * int
                   | Colon of int * int
                   | Comma of int * int
                   | Dblquote of int * int
                   | Decl of int * int
                   | Dot of int * int
                   | Else of int * int
                   | End of int * int
                   | EndOfFile of int * int
                   | Exn of int * int
                   | Function of int * int
                   | Handle of int * int
                   | Identifier of int * int * UniChar.Data
                   | If of int * int
                   | In of int * int
                   | Integer of int * int * int
                   | LBrace of int * int
                   | LBrack of int * int
                   | List of int * int
                   | LParen of int * int
                   | Mapsto of int * int
                   | Module of int * int
                   | Pipe of int * int
                   | Raise of int * int
                   | RBrace of int * int
                   | RBrack of int * int
                   | RParen of int * int
                   | String of int * int * UniChar.Data
                   | Then of int * int
                   | Type of int * int
                   | Val of int * int

   (* The current position in the input file. *)
   val lineno = ref 1
   val column = ref 0

   val reserved = [0wx22, 0wx23, 0wx28, 0wx29, 0wx2c, 0wx2e, 0wx3a, 0wx5b, 0wx5d, 0wx7b,
                   0wx7c, 0wx7d, 0wx0192, 0wx028b, 0wx03c4, 0wx2130, 0wx2133, 0wx2190,
                   0wx2192, 0wx22a5]

   (* raises: UnknownToken for any invalid tokens
    * returns: Tokens.token * DecFile
    *)
   fun nextToken file = let
      fun isReserved ch =
         List.exists (fn ele => UniChar.compareChar (ele, ch) = EQUAL) reserved

      (* raises: DecEof if eof is seen
       *         DecError for miscellaneous decoding errors
       * returns: UniChar.Char * DecFile
       *)
      fun readChar file = let
         val (ch, file') = Decode.decGetChar file
      in
         if ch = 0wxa then ( lineno := !lineno + 1 ; column := 0 ; (ch, file') )
         else ( column := !column + 1 ; (ch, file') )
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
         else file
      end

      (* Read a block of characters out of the file into a vector.  The end of
       * the block (or word - no, not the numeric type) is determined by the
       * isMember function.  If EOF is reached, the exn will be propagated back
       * up and handled by nextToken.
       *)
      fun readWord (lst, file, isMember) = let
         val (ch, file') = readChar file
      in
         if not (isMember ch) then ( column := !column - 1 ; (rev lst, file) )
         else readWord (ch::lst, file', isMember)
      end

      (* Figure out what kind of word we've read.  The easy way is to convert
       * it into a string and see if it matches any of our reserved words.  If
       * not, must be some crazy new user-defined identifier.
       *)
      fun handleWord lst =
         case UniChar.Data2String lst of
            "case"   => Case(!lineno, !column)
          | "decl"   => Decl(!lineno, !column)
          | "else"   => Else(!lineno, !column)
          | "end"    => End(!lineno, !column)
          | "f"      => Boolean(!lineno, !column, false)
          | "handle" => Handle(!lineno, !column)
          | "if"     => If(!lineno, !column)
          | "in"     => In(!lineno, !column)
          | "list"   => List(!lineno, !column)
          | "raise"  => Raise(!lineno, !column)
          | "t"      => Boolean(!lineno, !column, true)
          | "then"   => Then(!lineno, !column)
          | _        => Identifier(!lineno, !column, lst)

      fun handleInteger lst =
         case Int.fromString (UniChar.Data2String lst) of
             SOME i => i
           | NONE   => raise Error.ParseError ("FIXME", !lineno, !column, "Unable to perform numeric conversion.")

      (* Tokenizing strings is the hardest part of this whole process because
       * of the escape sequences, multiple lines, etc.  Too bad they can't be
       * as simple as they were originally designed to be.
       *)
      fun readString (str, file) = let
         (* Converts the string sequence \uXXXX into a single unicode character. *)
         fun readEscapedUnicode (lst, file) = let
            fun list2Int lst =
               StringCvt.scanString (Int.scan StringCvt.HEX) (UniChar.Data2String lst)
         in
            if List.length lst = 4 then
               case list2Int (rev lst) of
                  SOME i => (Word.fromInt i, file)
                | NONE   => raise Error.ParseError ("FIXME", !lineno, !column, "Invalid escaped Unicode character sequence.")
            else
               let
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
         0wx2190 => (Assign(!lineno, !column), file')
       | 0wx22a5 => (Bottom(!lineno, !column), file')
       | 0wx3a   => (Colon(!lineno, !column), file')
       | 0wx2c   => (Comma(!lineno, !column), file')
       | 0wx23   => nextToken (skipComments file')
       | 0wx22   => let
                       val (startLine, startCol) = (!lineno, !column)
                       val (str, file'') = readString ([], file')
                    in
                       (String(startLine, startCol, str), file'')
                    end
       | 0wx2e   => (Dot(!lineno, !column), file')
       | 0wx2130 => (Exn(!lineno, !column), file')
       | 0wx0192 => (Function(!lineno, !column), file')
       | (0wx30|0wx31|0wx32|0wx33|0wx34|0wx35|0wx36|0wx37|0wx38|0wx39) => let
                       val (str, file'') = readWord ([ch], file', fn ch => UniClasses.isDec ch)
                    in
                       (Integer(!lineno, !column, handleInteger str), file'')
                    end
       | 0wx7b   => (LBrace(!lineno, !column), file')
       | 0wx5b   => (LBrack(!lineno, !column), file')
       | 0wx28   => (LParen(!lineno, !column), file')
       | 0wx2192 => (Mapsto(!lineno, !column), file')
       | 0wx2133 => (Module(!lineno, !column), file')
       | 0wx7c   => (Pipe(!lineno, !column), file')
       | 0wx7d   => (RBrace(!lineno, !column), file')
       | 0wx5d   => (RBrack(!lineno, !column), file')
       | 0wx29   => (RParen(!lineno, !column), file')
       | 0wx03c4 => (Type(!lineno, !column), file')
       | 0wx028b => (Val(!lineno, !column), file')
       | _       => let
                       val (str, file'') = readWord ([ch], file', fn ch => not (UniClasses.isS ch) andalso not (isReserved ch))
                    in
                       (handleWord str, file'')
                    end
   end handle DecEof => (EndOfFile(!lineno, !column), file)
end
