/* This file defines the public interface to the tokenizer, which breaks up
 * the input file into a stream of tokens for use by the parser.
 *
 * $Id: tokens.h,v 1.10 2004/10/23 19:17:40 chris Exp $
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

#include "basic_types.h"

typedef enum { BOOLEAN, IDENTIFIER, INTEGER, LIST, STRING,
               ASSIGN, CASE, COLON, COMMA, DECL, DOT, ELSE, END,
               FUNCTION, IF, IN, LBRACE, LBRACK, LPAREN, MAPSTO, MODULE,
               RBRACE, RBRACK, RPAREN, THEN, TYPE, VAR,
               COMMENT, DBLQUOTE, ENDOFFILE } token_val_t;

/* How the lexer represents a token. */
typedef struct {
   unsigned int lineno;
   token_val_t type;       /* can be any value from the defines below */

   union {
      mint_t      integer;
      mstring_t   string;
      mbool_t     boolean;
   };
} token_t;

/* Maps a token's enum value to a descriptive string. */
extern const char *token_map[];

/* Return the next token from the previously opened file f, or NULL if no
 * more tokens are available.
 */
token_t *next_token (FILE *f);

#endif

/* vim: set tags=../tags: */
