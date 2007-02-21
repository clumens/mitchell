(* betterinfo-sig.sml - Definition of replacement GetOpt struct
 *
 * This signature includes the GetOpt signature, so that a structure of
 * type GET_OPT' can be used as a drop-in replacement for GetOpt, and adds
 * the help function, which provides a more versatile usageInfo.
 *
 * $Id: betterinfo-sig.sml,v 1.2 2004/08/05 23:47:38 das-cvs Exp $
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

signature GET_OPT =
sig
   include GET_OPT

   (* Rather than basing column positions on the size of the options column
    * and writing help all on one line, this function attempts to wrap 
    * everything within the given addresses.
    *
    * Raises Span if the help column starts before the opt column or if either
    * column has a negative length.  They can overlap, but it'll probably
    * look mighty strange.
    *)
   val help : {optStart : int, optEnd : int, helpStart : int, helpEnd : int}
               -> {header : string, options : 'a opt_descr list, 
                     footer : string} -> string
end
