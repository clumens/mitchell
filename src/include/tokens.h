/* This file defines the public interface to the tokenizer, which breaks up
 * the input file into a stream of tokens for use by the parser.
 *
 * $Id: tokens.h,v 1.5 2004/10/13 14:02:35 chris Exp $
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

/* More complicated values. */
#define  BOOLEAN     100      /* t or f */
#define  IDENTIFIER  101      /* a word identifier */
#define  INTEGER     102      /* an integer */
#define  LIST        103
#define  STRING      104      /* a string */

/* Reserved character and word values. */
#define  ASSIGN      200      /* ← */
#define  CASE        201      /* case */
#define  COLON       202      /* : */
#define  COMMA       203      /* , */
#define  CONST       204      /* ɕ */
#define  DECL        205      /* decl */
#define  DOT         206      /* . */
#define  ELSE        207      /* else */
#define  END         208      /* end */
#define  FUNCTION    209      /* ƒ */
#define  IF          210      /* if */
#define  IN          211      /* in */
#define  LBRACK      212      /* [ */
#define  LPAREN      213      /* ( */
#define  MAPSTO      214      /* → */
#define  MODULE      215      /* ℳ */
#define  RBRACK      216      /* ] */
#define  RPAREN      217      /* ) */
#define  THEN        218      /* then */
#define  TYPE        219      /* τ */
#define  VAR         220      /* ʋ */

#define  COMMENT     300      /* # */
#define  DBLQUOTE    301      /* " */

/* Return the next token from the previously opened file f, or NULL if no
 * more tokens are available.
 */
token_t *next_token (FILE *f);

#endif
