/* Convert the full-featured AST into a simplified but semantically equivalent
 * AST.  This process involves multiple passes over the tree.  However, there
 * is only a single entry point into the desugaring process as it deals with
 * the order of the passes internally.
 *
 * $Id: desugar.h,v 1.7 2005/04/27 02:05:12 chris Exp $
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
#ifndef _DESUGAR_H
#define _DESUGAR_H 1

#include "absyn.h"
#include "basic_types.h"

/* Entry point into the desugaring passes. */
ast_t *desugar_ast (ast_t *ast);

/* Individual passes - don't need to worry with these. */
ast_t *desugar_case_exprs (ast_t *ast);
ast_t *desugar_decl_exprs (ast_t *ast);

backlink_t *find_lexical_parent (backlink_t *bl);
backlink_t *make_bl (link_type kind, void *node);
absyn_id_expr_t *str_to_id_expr (mstring_t *str, unsigned int lineno,
                                 unsigned int column);

#endif

/* vim: set tags=../tags: */