/* Memory management macros and functions.
 *
 * $Id: memory.h,v 1.6 2005/08/10 01:40:09 chris Exp $
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
#ifndef _MEMORY_H
#define _MEMORY_H 1

#include <gc.h>

#include "config.h"
#include "error.h"
#include "translate.h"

#define MALLOC(ptr, size) \
   do { \
      if (((ptr) = GC_MALLOC(size)) == NULL) \
         MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__, N_("Error allocating memory.\n")); \
   } while (0)

#define REALLOC(ptr, size) \
   do { \
      if (((ptr) = GC_REALLOC(ptr, size)) == NULL) \
         MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__, N_("Error allocating memory.\n")); \
   } while (0)

#endif

/* vim: set tags=../tags: */
