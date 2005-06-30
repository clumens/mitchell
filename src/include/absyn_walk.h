/* Generic abstract syntax tree walking functions.  These functions perform
 * only the most basic of functions by walking the AST and visiting each node.
 * Override these basic versions with more complicated ones if that's what
 * a certain pass requires.
 *  
 * $Id: absyn_walk.h,v 1.1 2005/06/30 12:52:53 chris Exp $
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
#ifndef _ABSYN_WALK_H
#define _ABSYN_WALK_H 1

#include "absyn.h"

/* Yes, I am usually against using cpp to make variables and function
 * prototypes.  This is just too tedious to do any other way, though.  The
 * first one defines a pointer to an AST walking function of the given name
 * and type.  The second one defines that function's prototype.
 */
#define TRAV_FUNC_PTR(name, type) \
   type (* name)(struct absyn_funcs_t *funcs, type node)

#define TRAV_FUNC_PROTO(name, type) \
   type name (absyn_funcs_t *funcs, type node)

typedef struct absyn_funcs_t {
   TRAV_FUNC_PTR(visit_decl_expr, absyn_decl_expr_t *);
   TRAV_FUNC_PTR(visit_decl_lst, list_t *);
   TRAV_FUNC_PTR(visit_exn_handler, absyn_exn_handler_t *);
   TRAV_FUNC_PTR(visit_exn_lst, list_t *);
   TRAV_FUNC_PTR(visit_exn_expr, absyn_exn_expr_t *);
   TRAV_FUNC_PTR(visit_expr, absyn_expr_t *);
   TRAV_FUNC_PTR(visit_expr_lst, list_t *);
   TRAV_FUNC_PTR(visit_fun_call, absyn_fun_call_t *);
   TRAV_FUNC_PTR(visit_fun_decl, absyn_fun_decl_t *);
   TRAV_FUNC_PTR(visit_if_expr, absyn_if_expr_t *);
   TRAV_FUNC_PTR(visit_module_decl, absyn_module_decl_t *);
   TRAV_FUNC_PTR(visit_record_assn, list_t *);
   TRAV_FUNC_PTR(visit_record_ref, absyn_record_ref_t *);
   TRAV_FUNC_PTR(visit_val_decl, absyn_val_decl_t *);
} absyn_funcs_t;

TRAV_FUNC_PROTO(visit_decl_expr, absyn_decl_expr_t *);
TRAV_FUNC_PROTO(visit_decl_lst, list_t *);
TRAV_FUNC_PROTO(visit_exn_handler, absyn_exn_handler_t *);
TRAV_FUNC_PROTO(visit_exn_lst, list_t *);
TRAV_FUNC_PROTO(visit_exn_expr, absyn_exn_expr_t *);
TRAV_FUNC_PROTO(visit_expr, absyn_expr_t *);
TRAV_FUNC_PROTO(visit_expr_lst, list_t *);
TRAV_FUNC_PROTO(visit_fun_call, absyn_fun_call_t *);
TRAV_FUNC_PROTO(visit_fun_decl, absyn_fun_decl_t *);
TRAV_FUNC_PROTO(visit_if_expr, absyn_if_expr_t *);
TRAV_FUNC_PROTO(visit_module_decl, absyn_module_decl_t *);
TRAV_FUNC_PROTO(visit_record_assn, list_t *);
TRAV_FUNC_PROTO(visit_record_ref, absyn_record_ref_t *);
TRAV_FUNC_PROTO(visit_val_decl, absyn_val_decl_t *);

/* Set up pointers to all the generic AST walking functions.  Call this first,
 * then override with the pass-local functions.
 */
absyn_funcs_t *init_default_funcs ();

#endif

/* vim: set tags=../tags: */
