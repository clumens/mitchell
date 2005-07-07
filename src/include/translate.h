/* Help for string translations.
 *
 * $Id: translate.h,v 1.1 2005/07/07 05:04:14 chris Exp $
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
#ifndef _TRANSLATE_H
#define _TRANSLATE_H 1

#include <libintl.h>

#define _(String)             gettext(String)
#define gettext_noop(String)  String
#define N_(String)            gettext_noop(String)

#endif

/* vim: set tags=../tags: */
