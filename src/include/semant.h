/* Semantic analysis - symbol table management, type checking, etc.
 *
 * $Id: semant.h,v 1.3 2004/12/12 17:39:41 chris Exp $
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
#ifndef _SEMANT_H
#define _SEMANT_H 1

#include <stdio.h>

#include "absyn.h"
#include "basic_types.h"

void check_program (ast_t *ast);
wchar_t *ty_to_str (const ty_t *ty);

#endif

/* vim: set tags=../tags: */
