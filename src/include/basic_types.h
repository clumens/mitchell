/* Basic mitchell types, defined in terms of C types.
 *
 * $Id: basic_types.h,v 1.3 2004/11/30 02:13:07 chris Exp $
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
#ifndef _BASIC_TYPES_H
#define _BASIC_TYPES_H 1

#include <wchar.h>

typedef long int     mint_t;
typedef wchar_t      mstring_t;
typedef unsigned int mbool_t;

/* These are obvious except for a couple:
 *    TY_ALIAS is when you make a new name for an existing type,
 *    TY_BOTTOM is the type for when there's no other type (⊥)
 */
typedef enum { TY_ALIAS, TY_BOOLEAN, TY_BOTTOM, TY_INTEGER, TY_LIST,
               TY_RECORD, TY_STRING } ty_kind;

typedef struct ty_t {
   ty_kind ty;
} ty_t;

#endif

/* vim: set tags=../tags: */
