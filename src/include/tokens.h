/* This file defines the public interface to the tokenizer, which breaks up
 * the input file into a stream of tokens for use by the parser.
 *
 * $Id: tokens.h,v 1.2 2004/08/31 15:43:24 chris Exp $
 */

/* mitchell - the bootstrapping compiler
 * Copyright (C) 2004 Chris Lumens
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
 */
#ifndef _TOKENS_H
#define _TOKENS_H 1

#include <stdlib.h>
#include <wchar.h>

/* How the lexer represents a token. */
typedef struct {
   int type;               /* can be any value from the defines below */

   union {
      long     integer;
      wchar_t *string;
   };
} token_t;

/* More complicated values. */
#define  IDENTIFIER  100      /* a word identifier */
#define  INTEGER     101      /* an integer */
#define  STRING      102      /* a string */

/* Reserved character values. */
#define  COMMENT     200      /* # */
#define  LPAREN      201      /* ( */
#define  RPAREN      202      /* ) */
#define  LBRACK      203      /* [ */
#define  RBRACK      204      /* ] */
#define  COMMA       205      /* , */
#define  DBLQUOTE    206      /* " */

/* Return the next token from the previously opened file f, or NULL if no
 * more tokens are available.
 */
token_t *next_token (FILE *f);

#endif
