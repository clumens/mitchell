/* This file defines the public interface to the parser, which takes the token
 * stream produced by the tokenizer and matches it to the language's grammer.
 * This is done for both validation of the input file and for building the
 * abstract syntax tree.
 *
 * $Id: parse.h,v 1.4 2005/07/07 05:04:14 chris Exp $
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
#ifndef _PARSE_H
#define _PARSE_H 1

#include "absyn.h"

#ifdef __cplusplus
   extern "C" {
#endif

/* Parse the file provided by the caller.  Returns a pointer to the root node
 * of the abstract syntax tree.
 */
ast_t *parse (char *filename);

#ifdef __cplusplus
   }
#endif

#endif

/* vim: set tags=../tags: */
