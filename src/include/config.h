/* This file defines the structures that hold all the runtime configuration
 * of the compiler.  For the most part, this configuration is set by the
 * command line options provided by the user.
 *
 * $Id: config.h,v 1.4 2005/01/07 05:31:21 chris Exp $
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
#ifndef _CONFIG_H
#define _CONFIG_H 1

typedef struct {
   char *filename;                     /* the file we're compiling */

   struct {
      unsigned int parser_debug;       /* debug output level for the parser */

      unsigned int dump_absyn;         /* dump the abstract syntax or not? */
      char *absyn_outfile;             /* if dump_absyn is set, the dest */
      unsigned int dump_symtab;        /* dump the symbol tables or not? */
      char *symtab_outfile;            /* if dump_symtab is set, the dest */
   } debug;
} compiler_config_t;

/* Here's the one instance of this structure we need. */
extern compiler_config_t compiler_config;

#endif

/* vim: set tags=../tags: */
