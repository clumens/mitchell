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
 * - rename all values and functions to be globally unique
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
 * $Id: desugar.c,v 1.1 2005/02/11 01:38:30 chris Exp $
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
#include "desugar.h"
#include "memory.h"
#include "str.h"

/* Entry point into the desugaring passes. */
ast_t *desugar_ast (ast_t *ast)
{
   return desugar_decl_exprs (ast);
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
         case LINK_FUN_DECL:
         case LINK_MODULE_DECL:
            return tmp;

         case LINK_BRANCH_LST:
            tmp = ((absyn_branch_lst_t *) tmp->ptr)->parent; break;

         case LINK_CASE_EXPR:
            tmp = ((absyn_case_expr_t *) tmp->ptr)->parent; break;

         case LINK_DECL:
            tmp = ((absyn_decl_t *) tmp->ptr)->parent; break;

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

static unsigned long int unique_id = 0;

/* Construct a new function symbol that is guaranteed to not overlap with
 * anything the source file contains.  These symbols all start with a #, which
 * is the comment character and therefore will be stripped out by the
 * tokenizer.
 */
absyn_id_expr_t *make_unique_sym (mstring_t *base, unsigned int lineno,
                                  unsigned int column)
{
   absyn_id_expr_t *retval;
   char *buf;

   MALLOC (buf, sizeof(char)*10);
   snprintf (buf, 9, "%ld", unique_id);

   MALLOC (retval, sizeof(absyn_id_expr_t));
   retval->lineno = lineno;
   retval->column = column;
   retval->symbol = build_wcsstr (4, L"#", base, L"_", buf);
   retval->sub = NULL;

   unique_id++;

   return retval;
}
