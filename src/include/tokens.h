/* This file defines the public interface to the tokenizer, which breaks up
 * the input file into a stream of tokens for use by the parser.
 *
 * $Id: tokens.h,v 1.7 2004/10/15 14:36:50 chris Exp $
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
   unsigned int lineno;
   int type;               /* can be any value from the defines below */

   union {
      long          integer;
      wchar_t      *string;
      unsigned int  boolean;
   };
} token_t;

extern const char *token_map[];

/* More complicated values. */
#define  BOOLEAN     0        /* t or f */
#define  IDENTIFIER  1        /* a word identifier */
#define  INTEGER     2        /* an integer */
#define  LIST        3
#define  STRING      4        /* a string */

/* Reserved character and word values. */
#define  ASSIGN      5        /* ← */
#define  CASE        6        /* case */
#define  COLON       7        /* : */
#define  COMMA       8        /* , */
#define  CONST       9        /* ɕ */
#define  DECL        10       /* decl */
#define  DOT         11       /* . */
#define  ELSE        12       /* else */
#define  END         13       /* end */
#define  FUNCTION    14       /* ƒ */
#define  IF          15       /* if */
#define  IN          16       /* in */
#define  LBRACE      17       /* { */
#define  LBRACK      18       /* [ */
#define  LPAREN      19       /* ( */
#define  MAPSTO      20       /* → */
#define  MODULE      21       /* ℳ */
#define  RBRACE      22       /* } */
#define  RBRACK      23       /* ] */
#define  RPAREN      24       /* ) */
#define  THEN        25       /* then */
#define  TYPE        26       /* τ */
#define  VAR         27       /* ʋ */

#define  COMMENT     28       /* # */
#define  DBLQUOTE    29       /* " */

#define  ENDOFFILE   30

/* Return the next token from the previously opened file f, or NULL if no
 * more tokens are available.
 */
token_t *next_token (FILE *f);

#endif

/* vim: set tags=../tags: */
