/* Perform lambda lifting, which is the process of taking all functions and
 * raising them up to the same level.  This eliminates nested functions so
 * conversion to IR (and eventually machine code) may be performed.  One of the
 * major parts of lambda lifting is performing free value analysis, where
 * all functions must be modified to accept additional parameters.  These
 * parameters are all unbound values in the function.  In this way, the
 * function has all values it needs regardless of enclosing scope and may
 * then be lifted.
 *
 * This is a good pass to come near the end.  It shouldn't come last since
 * that's where we may perform any cleanups required by the rest of the
 * desugarings, but could come immediately before that pass.
 * 
 * $Id: free_vals.c,v 1.8 2005/08/22 23:03:06 chris Exp $
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
#include <string.h>
#include <wchar.h>

#include "absyn.h"
#include "absyn_walk.h"
#include "config.h"
#include "desugar.h"
#include "error.h"
#include "list.h"
#include "memory.h"
#include "translate.h"

static absyn_expr_t *lift_visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node, void **user_data);
static absyn_fun_call_t *lift_visit_fun_call (absyn_funcs_t *funcs, absyn_fun_call_t *node, void **user_data);
static absyn_fun_decl_t *lift_visit_fun_decl (absyn_funcs_t *funcs, absyn_fun_decl_t *node, void **user_data);

static unsigned int pass = 0;
static unsigned int changed;
static FILE *out = NULL;

/* Entry point for this pass. */
ast_t *lift_functions (absyn_funcs_t *funcs, ast_t *ast)
{
   list_t *tmp;

   /* Free value analysis is an iterative process - after we have found the
    * free values in one function and added them as parameters to all its
    * calls, we might then have made those same values free in the caller.  So
    * now we need to repeat the checks.
    *
    * Possible optimization - mark a fun-decl AST node if it has no calls and
    * if so, bail on checking.
    */
   do {
      changed = 0;

      if (cconfig.debug.dump_free_vals)
      {
         fprintf (out, _("Free value analysis pass %d\n"), pass);
         fprintf (out, "===========================\n");
      }

      for (tmp = ast; tmp != NULL; tmp = tmp->next)
         tmp->data = funcs->visit_module_decl (funcs, tmp->data, NULL);

      pass++;
   } while (changed);

   return ast;
}

/* Initialization for this pass. */
absyn_funcs_t *init_lift_pass()
{
   absyn_funcs_t *retval = init_default_funcs();
   retval->visit_expr = lift_visit_expr;
   retval->visit_fun_call = lift_visit_fun_call;
   retval->visit_fun_decl = lift_visit_fun_decl;

   if (cconfig.debug.free_val_outfile == NULL || strcmp ("-", cconfig.debug.free_val_outfile) == 0)
      out = stdout;
   else
   {
      if ((out = fopen (cconfig.debug.free_val_outfile, "w")) == NULL)
      {
         COULD_NOT_WRITE_ERROR (cconfig.debug.free_val_outfile);
         exit(1);
      }
   }
   
   return retval;
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

static int __id_to_str_cmp (void *lst_data, void *user_data)
{
    return wcscmp (((absyn_id_expr_t *) lst_data)->symbol, (wchar_t *) user_data);
}

static symtab_t *get_parent_symtab (backlink_t *parent)
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
         MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__,
                                  N_("Node's parent does not contain a symbol table.\n"));
   }

   return NULL;
}

static unsigned int is_free (symtab_t *symtab, mstring_t *name)
{
   return table_lookup_entry (symtab, name, SYM_VALUE) == NULL && table_lookup_entry (symtab, name, SYM_EXN) == NULL;
}

static void report_free (absyn_id_expr_t *node)
{
   absyn_id_expr_t *tmp;

   fprintf (out, "  %d:%d: ", node->lineno, node->column);

   for (tmp = node; tmp != NULL; tmp = tmp->sub)
   {
      if (tmp->sub == NULL)
         fprintf (out, "%ls", tmp->symbol);
      else
         fprintf (out, "%ls.", tmp->symbol);
   }
}

/* +================================================================+
 * | PASS-LOCAL AST WALKING FUNCTIONS                               |
 * +================================================================+
 */

static absyn_expr_t *lift_visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node, void **user_data)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
      case ABSYN_BOTTOM:
      case ABSYN_INTEGER:
      case ABSYN_STRING:
         break;

      case ABSYN_DECL:
         node->decl_expr = funcs->visit_decl_expr (funcs, node->decl_expr, user_data);
         break;

      case ABSYN_EXN:
         node->exn_expr = funcs->visit_exn_expr (funcs, node->exn_expr, user_data);
         break;

      case ABSYN_EXPR_LST:
         node->expr_lst = funcs->visit_expr_lst (funcs, node->expr_lst, user_data);
         break;

      case ABSYN_FUN_CALL:
         node->fun_call_expr = funcs->visit_fun_call (funcs, node->fun_call_expr, user_data);
         break;

      /* We only need to check values to see if they're bound or not.  Also,
       * we don't need to check values that we created in other simplification
       * passes because those can never be free (it would be a good idea to
       * sit down and prove this assumption).
       */
      case ABSYN_ID:
         if (node->identifier->kind == SYM_VALUE && wcsncmp(node->identifier->symbol, L"_.__", 4) != 0)
         {
            backlink_t *parent = find_lexical_parent (node->parent);
            symtab_t *symtab = get_parent_symtab (parent);

            /* Values from another module are a special case.  For now, we'll assume they are bound. */
            if (node->identifier->sub != NULL)
               break;

            /* If the value is not found in the innermost symtab, it's free. */
            if (is_free (symtab, node->identifier->symbol))
            {
               /* However, exception handlers are weird.  The exception value
                * is placed into a new environment, but the handler itself does
                * not exist in a new level of scope.  So we also need to check
                * the next outer symbol table.  Yes, this is kind of stupid.
                */
               if (parent->kind == LINK_EXN_LST)
               {
                  backlink_t *gparent = find_lexical_parent (((absyn_exn_lst_t *) parent->ptr)->parent);
                  symtab_t *gsymtab = get_parent_symtab (gparent);
                     
                  if (is_free (gsymtab, node->identifier->symbol))
                     *user_data = list_append (*user_data, node->identifier);
               }
               else
                  *user_data = list_append (*user_data, node->identifier);
            }
         }

         break;

      case ABSYN_IF:
         node->if_expr = funcs->visit_if_expr (funcs, node->if_expr, user_data);
         break;

      case ABSYN_RAISE:
         node->raise_expr = funcs->visit_expr (funcs, node->raise_expr, user_data);
         break;

      case ABSYN_RECORD_ASSN:
         node->record_assn_lst = funcs->visit_record_assn (funcs, node->record_assn_lst, user_data);
         break;

      case ABSYN_RECORD_REF:
         node->record_ref = funcs->visit_record_ref (funcs, node->record_ref, user_data);
         break;

      /* Running into a case expression is impossible, but this shuts up gcc. */
      case ABSYN_CASE:
#ifndef NEW_GRAMMAR
      default:
#endif
         MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__, N_("New AST expr node type not handled.\n"));
   }

   if (node->exn_handler != NULL)
      node->exn_handler = funcs->visit_exn_handler (funcs, node->exn_handler, user_data);

   return node;
}

static absyn_fun_call_t *lift_visit_fun_call (absyn_funcs_t *funcs, absyn_fun_call_t *node, void **user_data)
{
   list_t *tmp;
   backlink_t *parent = find_lexical_parent (node->parent);
   symtab_t *symtab = get_parent_symtab (parent);

   /* Handle all the actual parameters first. */
   for (tmp = node->arg_lst; tmp != NULL; tmp = tmp->next)
      tmp->data = funcs->visit_expr (funcs, tmp->data, user_data);

   /* Now check each of the free values we need to pass to the function call to make sure they're
    * bound in this scope.  If not, it means we'll have to pass them to the caller's caller as well.
    * See comments in lift_visit_expr for explanation.
    */
   for (tmp = node->free_vals; tmp != NULL; tmp = tmp->next)
   {
      absyn_id_expr_t *tmp_id = tmp->data;

      if (is_free (symtab, tmp_id->symbol))
      {
         if (parent->kind == LINK_EXN_LST)
         {
            backlink_t *gparent = find_lexical_parent (((absyn_exn_lst_t *) parent->ptr)->parent);
            symtab_t *gsymtab = get_parent_symtab (gparent);

            if (is_free (gsymtab, tmp_id->symbol))
               *user_data = list_append (*user_data, node->identifier);
         }
         else
            *user_data = list_append (*user_data, tmp_id);
      }
   }

   return node;
}

static absyn_fun_decl_t *lift_visit_fun_decl (absyn_funcs_t *funcs, absyn_fun_decl_t *node, void **user_data)
{
   list_t   *free_values = NULL;
   list_t   *prev_free_values = NULL;
   list_t   *tmp, *tmp2;

   /* Save free_values list for calling function. */
   if (user_data == NULL)
      prev_free_values = NULL;
   else
      prev_free_values = *user_data;

   node->body = funcs->visit_decl_expr (funcs, node->body, (void *) &free_values);

   if (cconfig.debug.dump_free_vals != 0)
      fprintf (out, _("Free values in function %ls:"), node->symbol->symbol);

   /* Iterate over list of free values. */
   for (tmp = free_values; tmp != NULL; tmp = tmp->next)
   {
      absyn_id_expr_t *val = tmp->data;

      if (cconfig.debug.dump_free_vals != 0)
         report_free (val);

      /* Traverse list of callers of this function, appending the free values to the caller's list if it's not
       * already on there.
       */
      for (tmp2 = node->uses; tmp2 != NULL; tmp2 = tmp2->next)
      {
         absyn_fun_call_t *caller = tmp2->data;
         
         if (list_find (caller->free_vals, val->symbol, __id_to_str_cmp) == NULL)
         {
            caller->free_vals = list_append (caller->free_vals, val);
            changed = 1;
         } 
      }
   }

   if (cconfig.debug.dump_free_vals != 0)
      fprintf (out, "\n");

   /* Restore caller's free_values list. */
   if (user_data != NULL)
      *user_data = prev_free_values;
   
   return node;
}

/* vim: set tags=../tags: */
