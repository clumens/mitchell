/* Basic mitchell types, defined in terms of C types.
 *
 * $Id: basic_types.h,v 1.2 2004/11/24 03:56:03 chris Exp $
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

#endif

/* vim: set tags=../tags: */
