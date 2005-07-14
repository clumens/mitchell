/* Perform lambda lifting, which is the process of taking all functions and
 * raising them up to the same level.  This eliminates nested functions so
 * conversion to IR (and eventually machine code) may be performed.  One of
 * the major parts of lambda lifting is performing free variable analysis,
 * where all functions must be modified to accept additional parameters.  These
 * parameters are all unbound variables in the function.  In this way, the
 * function has all variables it needs regardless of enclosing scope and may
 * then be lifted.
 *
 * This is a good pass to come near the end.  It shouldn't come last since
 * that's where we may perform any cleanups required by the rest of the
 * desugarings, but could come immediately before that pass.
 * 
 * $Id: free_vals.c,v 1.4 2005/07/14 01:59:17 chris Exp $
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
#include "desugar.h"
#include "error.h"
#include "list.h"
#include "memory.h"
#include "translate.h"

static absyn_expr_t *lift_visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node);

/* Entry point for this pass. */
ast_t *lift_functions (absyn_funcs_t *funcs, ast_t *ast)
{
   list_t *tmp;

   for (tmp = ast; tmp != NULL; tmp = tmp->next)
      tmp->data = funcs->visit_module_decl (funcs, tmp->data);

   return ast;
}

/* Initialization for this pass. */
absyn_funcs_t *init_lift_pass()
{
   absyn_funcs_t *retval = init_default_funcs();
   retval->visit_expr = lift_visit_expr;
   return retval;
}

static symtab_t *get_symtab (absyn_expr_t *node, backlink_t *parent)
{
   switch (parent->kind) {
      case LINK_DECL_EXPR:
         return ((absyn_decl_expr_t *) parent->ptr)->symtab;

      case LINK_EXN_LST:
         return ((absyn_exn_lst_t *) parent->ptr)->symtab;

      case LINK_FUN_DECL:
         return ((absyn_fun_decl_t *) parent->ptr)->symtab;

      case LINK_MODULE_DECL:
         return ((absyn_module_decl_t *) parent->ptr)->symtab;

      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, _("bad parent->kind"),
                                  __FILE__, __LINE__);
         exit(1);
   }
}

/* +================================================================+
 * | PASS-LOCAL AST WALKING FUNCTIONS                               |
 * +================================================================+
 */

static absyn_expr_t *lift_visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
      case ABSYN_BOTTOM:
      case ABSYN_INTEGER:
      case ABSYN_STRING:
         break;

      case ABSYN_DECL:
         node->decl_expr = funcs->visit_decl_expr (funcs, node->decl_expr);
         break;

      case ABSYN_EXN:
         node->exn_expr = funcs->visit_exn_expr (funcs, node->exn_expr);
         break;

      case ABSYN_EXPR_LST:
         node->expr_lst = funcs->visit_expr_lst (funcs, node->expr_lst);
         break;

      case ABSYN_FUN_CALL:
         node->fun_call_expr = funcs->visit_fun_call (funcs,
                                                      node->fun_call_expr);
         break;

      case ABSYN_ID:
      {
         backlink_t *parent = find_lexical_parent (node->parent);
         symtab_t *symtab = get_symtab (node, parent);
         unsigned int lineno = node->identifier->lineno;
         unsigned int column = node->identifier->column;

         /* We only need to check values to see if they're bound or not. */
         if (node->identifier->kind != SYM_VALUE)
            break;

         /* Values that refer to another module are always free. */
         if (node->identifier->sub != NULL)
         {
            absyn_id_expr_t *tmp;
            
            fprintf (stderr, _("%d:%d free value:  \n"), lineno, column);

            for (tmp = node->identifier; tmp != NULL; tmp = tmp->sub)
            {
               if (tmp->sub == NULL)
                  fprintf (stderr, "%ls", tmp->symbol);
               else
                  fprintf (stderr, "%ls.", tmp->symbol);
            }

            fprintf (stderr, "\n");
            break;
         }

         fprintf (stderr, "checking: %ls\n", node->identifier->symbol);

         /* If the value is not found in the innermost symtab, it's free. */
         if (table_lookup_entry (symtab, node->identifier->symbol,
                                 SYM_VALUE) == NULL &&
             table_lookup_entry (symtab, node->identifier->symbol,
                                 SYM_EXN) == NULL)
         {
            /* However, exception handlers are weird.  The exception value is
             * placed into a new environment, but the handler itself does not
             * exist in a new level of scope.  So we also need to check the
             * next outer symbol table.  Yes, this is kind of stupid.
             */
            if (parent->kind == LINK_EXN_LST)
            {
               backlink_t *gparent =
                  find_lexical_parent (((absyn_exn_lst_t *) parent->ptr)->parent);
               symtab_t *gsymtab = get_symtab(node, gparent);
                  
               if (table_lookup_entry (gsymtab, node->identifier->symbol,
                                       SYM_VALUE) == NULL &&
                   table_lookup_entry (gsymtab, node->identifier->symbol,
                                       SYM_EXN) == NULL)
                  fprintf (stderr, _("%d:%d free value:  %ls\n"),
                           lineno, column, node->identifier->symbol);
            }
            else
               fprintf (stderr, _("%d:%d free value:  %ls\n"),
                        lineno, column, node->identifier->symbol);
         }

         break;
      }

      case ABSYN_IF:
         node->if_expr = funcs->visit_if_expr (funcs, node->if_expr);
         break;

      case ABSYN_RAISE:
         node->raise_expr = funcs->visit_expr (funcs, node->raise_expr);
         break;

      case ABSYN_RECORD_ASSN:
         node->record_assn_lst =
            funcs->visit_record_assn (funcs, node->record_assn_lst);
         break;

      case ABSYN_RECORD_REF:
         node->record_ref = funcs->visit_record_ref (funcs, node->record_ref);
         break;

      /* Running into a case expression is impossible, but this shuts up gcc. */
      case ABSYN_CASE:
#ifndef NEW_GRAMMAR
      default:
#endif
         MITCHELL_INTERNAL_ERROR (cconfig.filename, _("bad node->kind"),
                                  __FILE__, __LINE__);
         exit(1);
   }

   if (node->exn_handler != NULL)
      node->exn_handler = funcs->visit_exn_handler (funcs, node->exn_handler);

   return node;
}

/* vim: set tags=../tags: */
