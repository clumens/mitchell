(* betterinfo.sml - Drop-in replacement for GetOpt with an improved usageInfo
 *
 * This function uses the smlnj-lib GetOpt for all of its functionality except
 * for usageInfo.  This usageInfo uses the help function as its backend, using
 * addresses of {optStart=3, optEnd=27, helpStart=30, helpEnd=79} and an empty
 * footer.
 *
 * See the file INSTALL for information on how to use this replacement GetOpt.
 *
 * Be aware that even though the datatypes in this GetOpt use the same
 * constructors as those in GetOpt, they are different types.  The structure
 * must be loaded before any types in GetOpt are used.
 *
 * $Id: betterinfo.sml,v 1.3 2004/08/06 03:59:55 das-cvs Exp $
 *
 * Copyright (c) 2004 David Shea <david@gophernet.org>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to 
 * deal in the Software without restriction, including without limitation the 
 * rights to use, copy, modifiy, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is 
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in 
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *)

structure GetOpt :> GET_OPT =
struct
   open GetOpt

   (* splitAt is a char -> bool function that returns true on characters
    * that can be used for a line boundary.  Whitespace is always stripped,
    * and in the case of a split on a non-whitespace character, the split
    * is made after the character.  So, for example, to split on whitespace
    * or hyphens, you could use
    *    fn c => (c = #"-") orelse (Char.isSpace c)
    *
    * If a particular line cannot be made equal to or shorter than len, then
    * the first possible split after len is made.
    *
    * Returns a list of substrings over str.
    *)
   fun wrap splitAt len str =
      let
         fun split substr =
            if ((Substring.size substr) < len) then [substr] else
               let
                  val splitTest = not o splitAt
                  (* Chop the substring off at len, search from the right
                   * for a split.
                   *)
                  val (a, b) = Substring.splitr splitTest
                                 (Substring.slice (substr, 0, SOME len))
               in
                  if (Substring.isEmpty a) then (* failed to split *)
                     (* Same thing, different direction *)
                     let val (a, b) = Substring.splitl splitTest
                                       (Substring.slice (substr, len, NONE))
                     in
                        (Substring.span (Substring.slice 
                                             (substr, 0, SOME len),
                                           Substring.dropr Char.isSpace a))
                           :: split (Substring.dropl Char.isSpace b)
                     end
                  else
                     (Substring.dropr Char.isSpace a) ::
                        (split 
                           (Substring.span 
                              (Substring.dropl Char.isSpace b,
                                  Substring.slice (substr, len, NONE))))
               end
      in
         (* Accepting len 0, which will result in splitting into one word per
          * line; sort of an excessively complicated tokenizer *)
         if (len < 0) then raise Span else
            split (Substring.full str)
      end

   fun help {optStart, optEnd, helpStart, helpEnd} 
                  {header, options, footer} =
      (* In case of overflow on col1, add a linebreak to the head of col2,
       * starting it the next line down from the overflowed column.  example
       * (using pipes to show limits):
       *
       *   |                       |  | 
       *   -v, --upgrade-version=VERSION
       *                              Choose VERSION for upgrading if multiple
       *                              versions of a package are installed
       *
       * Multiple arguments in the same opt_descr are handled by putting each
       * on a separate line.  If your long options are mnemonics for short
       * options, I suggest you line them up.  A descr that looks like:
       *
       *    {short="bnI", long=["binary", "not-text"], 
       *      desc=OptArg (_, "FILE"),...}
       *
       * might turn into
       *
       *    -b, --binary=[FILE]       Open FILE (or standard input if none
       *    -n, --not-test=[FILE]     given) in binary mode
       *    -I [FILE]
       *
       * If you use multiple longs in the same opt_descr and only overflow some
       * of them, it looks really gross.  So don't do that.
       *)
      let
         val optLen = 
            let val l = optEnd - optStart + 1
            in
               if (l < 0) then raise Span else l
            end

         val helpLen = 
            let val l = helpEnd - helpStart + 1
            in
               if (l < 0) then raise Span else l
            end

         (* Sanity checks *)
         val _ = if (optLen < 0) orelse (helpLen < 0) then raise Span else ()
         val _ = if (helpStart < optStart) then raise Span else ()

         (* 4 is room for a hyphen, letter, comma and a space.  e.g.,
          *   |   |
          *   -a, --all
          *)
         val longStart = optStart + 4
         val longLen = optEnd - longStart + 1
         
         fun pad i str =
            (String.implode (List.tabulate (i, (fn i => #" ")))) ^ str

         val mapShort = List.map (fn c => "-" ^ (String.str c))
         val mapLong = List.map (fn long => "--" ^ long)

         val wrapHelp = 
            wrap (fn c => (c = #"-") orelse (Char.isSpace c)) helpLen

         val rows =
            List.foldr
               (fn ({short, long, help, desc}, lst) =>
                  let val shorts = mapShort (String.explode short)
                      val longs = mapLong long
                      val helps = List.map (Substring.string) (wrapHelp help)

                      (* This function attempt to match each short option,
                       * long option, and line of help onto a single line
                       * of text.  Returns a list of lines as strings.
                       *)
                      fun join (slst, long :: ltail, hlst) =
                           let val long =
                                 case desc of
                                    NoArg _ => long
                                  | ReqArg (_, arg) =>
                                       long ^ "=" ^ arg
                                  | OptArg (_, arg) =>
                                       long ^ "[=" ^ arg ^ "]"
                               val llen = String.size long
                               val overflow = llen > longLen
                               val (short, stail) =
                                 if (null slst) then ("    ", slst)
                                 else ((hd slst) ^ ", ", tl slst)
                               val htail = 
                                 if (null hlst) then hlst else (tl hlst)
                               val htail = if overflow then hlst else htail
                           in
                              ((pad (optStart - 1) short)) ^
                                 long ^
                                 (if overflow then ""
                                  else if (null hlst) then ""
                                  else (pad (helpStart - (longStart + llen)) 
                                          (hd hlst)))
                                 ^ "\n"
                              :: join (stail, ltail, htail)
                           end
                        | join (short :: stail, nil, hlst) =
                           let val short =
                                 case desc of
                                    NoArg _ => short
                                  | ReqArg (_, arg) =>
                                       short ^ " " ^ arg
                                  | OptArg (_, arg) =>
                                       short ^ " [" ^ arg ^ "]"
                               val slen = String.size short
                               val overflow = slen > optLen
                               val htail =
                                 if (null hlst) then hlst else (tl hlst)
                               val htail = if overflow then hlst else htail
                           in
                              ((pad (optStart - 1) short)) ^
                                 (if overflow then ""
                                  else if (null hlst) then ""
                                  else 
                                    (pad (helpStart - (optStart + slen)) help))
                               ^ "\n"
                              :: join (stail, nil, htail)
                           end
                        | join (nil, nil, help :: htail) =
                           (pad (helpStart - 1) help) ^ "\n" :: 
                              join (nil, nil, htail)
                        | join (nil, nil, nil) = nil
                  in
                     (String.concat (join (shorts, longs, helps)))
                        :: lst
                  end ) [] options
      in
         (* NB: usageInfo adds a linebreak between header and the option
          * body; this does not
          *)
         header ^ (String.concat rows) ^ footer
      end

   fun usageInfo {header, options} =
      help {optStart=3, optEnd=27, helpStart=30, helpEnd=79}
         {header=header ^ "\n", options=options, footer=""}
end
