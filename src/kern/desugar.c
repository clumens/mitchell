/* Convert the full-featured AST into a simplified but semantically equivalent
 * AST.  This process involves multiple passes over the tree.  However, there
 * is only a single entry point into the desugaring process as it deals with
 * the order of the passes internally.
 *
 * General idea for ordering/purpose of passes:
 *
 * - case-expr transforms:
 *    - convert case-exprs consisting of only an else branch into an expr
 *    - convert all other case-exprs to chained if-exprs in a decl-decl
 * - remove type-decls
 * - decl-expr transforms:
 *    - convert decl-exprs with no decls into exprs
 *    - promote decl-exprs that are not the RHS of a function into
 *      functions
 * - rename symbols to be unique and usable as assembler symbol names
 * - lambda lifting to remove all nested functions:
 *    - eliminate free values:
 *       - add symbol tables to AST nodes representing a new level of
 *         scope
 *       - add "free values to bind" list to function symbols
 *       - discover free values by checking each value reference to see if
 *         it's defined in the local symbol table
 *       - add all free values to the function's symbol table entry
 *       - on function call, look up function symbol and append free value
 *         list to function arguments
 *    - lift all functions to module's top-level scope
 *
 * $Id: desugar.c,v 1.14 2005/07/14 03:02:52 chris Exp $
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
#include <stdio.h>
#include <stdlib.h>

#include "absyn.h"
#include "absyn_walk.h"
#include "config.h"
#include "desugar.h"
#include "error.h"
#include "memory.h"
#include "translate.h"

/* Entry point into the desugaring passes. */
ast_t *desugar_ast (ast_t *ast)
{
   absyn_funcs_t *walk_funcs;

   walk_funcs = init_case_pass();
   ast = desugar_case_exprs (walk_funcs, ast);
   if (cconfig.debug.dump_absyn)
      print_absyn (ast, &cconfig, _("case-removed abstract syntax tree"));

   if (cconfig.last_phase == LAST_DESUGAR_CASE)
      exit(0);
   
   walk_funcs = init_decl_pass();
   ast = desugar_decl_exprs (walk_funcs, ast);
   if (cconfig.debug.dump_absyn)
      print_absyn (ast, &cconfig, _("decl-promoted abstract syntax tree"));

   if (cconfig.last_phase == LAST_DESUGAR_DECL)
      exit(0);

   walk_funcs = init_lift_pass();
   ast = lift_functions (walk_funcs, ast);

   if (cconfig.last_phase == LAST_DESUGAR_LIFT)
      exit(0);

   return ast;
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

/* Given an AST node's backlink, follow the parent pointers up until we
 * get to one that points at a decl-expr, fun-decl, or module-decl.  These
 * types form new levels of scope and are therefore the lexical parent of
 * the current AST node.
 */
backlink_t *find_lexical_parent (backlink_t *bl)
{
   backlink_t *tmp = bl;

   while (tmp != NULL)
   {
      /* Aggravating. */
      switch (tmp->kind) {
         case LINK_DECL_EXPR:
         case LINK_EXN_LST:
         case LINK_FUN_DECL:
         case LINK_MODULE_DECL:
            return tmp;

         case LINK_BRANCH_LST:
            tmp = ((absyn_branch_lst_t *) tmp->ptr)->parent; break;

         case LINK_CASE_EXPR:
            tmp = ((absyn_case_expr_t *) tmp->ptr)->parent; break;

         case LINK_DECL:
            tmp = ((absyn_decl_t *) tmp->ptr)->parent; break;

         case LINK_EXN:
            tmp = ((absyn_exn_expr_t *) tmp->ptr)->parent; break;

         case LINK_EXN_HANDLER:
            tmp = ((absyn_exn_handler_t *) tmp->ptr)->parent; break;

         case LINK_EXPR:
            tmp = ((absyn_expr_t *) tmp->ptr)->parent; break;

         case LINK_FUN_CALL:
            tmp = ((absyn_fun_call_t *) tmp->ptr)->parent; break;

         case LINK_ID_EXPR:
            tmp = ((absyn_id_expr_t *) tmp->ptr)->parent; break;

         case LINK_ID_LST:
            tmp = ((absyn_id_lst_t *) tmp->ptr)->parent; break;

         case LINK_IF_EXPR:
            tmp = ((absyn_if_expr_t *) tmp->ptr)->parent; break;

         case LINK_RAISE:
            tmp = ((absyn_if_expr_t *) tmp->ptr)->parent; break;

         case LINK_RECORD_ASSN:
            tmp = ((absyn_record_assn_t *) tmp->ptr)->parent; break;

         case LINK_RECORD_REF:
            tmp = ((absyn_record_ref_t *) tmp->ptr)->parent; break;

         case LINK_TY:
            tmp = ((absyn_ty_t *) tmp->ptr)->parent; break;

         case LINK_TY_DECL:
            tmp = ((absyn_ty_decl_t *) tmp->ptr)->parent; break;

         case LINK_VAL_DECL:
            tmp = ((absyn_val_decl_t *) tmp->ptr)->parent; break;

#ifndef NEW_GRAMMAR
         default:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__,
                                     N_("New backlink type not handled.\n"));
#endif
      }
   }

   return NULL;
}

backlink_t *make_bl (link_type kind, void *node)
{
   backlink_t *retval;

   MALLOC (retval, sizeof(backlink_t));
   retval->kind = kind;
   retval->ptr = node;

   return retval;
}

/* Convert a string into an absyn_id_expr_t */
absyn_id_expr_t *str_to_id_expr (mstring_t *str, unsigned int lineno,
                                 unsigned int column)
{
   absyn_id_expr_t *retval;

   MALLOC (retval, sizeof(absyn_id_expr_t));
   retval->lineno = lineno;
   retval->column = column;
   retval->symbol = str;
   retval->label = str;
   retval->sub = NULL;

   return retval;
}

/* vim: set tags=../tags: */
