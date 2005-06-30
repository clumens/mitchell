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
 * $Id: free_vals.c,v 1.1 2005/06/30 00:56:04 chris Exp $
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
#include "error.h"
#include "list.h"

static absyn_decl_expr_t *handle_decl_expr (absyn_decl_expr_t *node);
static list_t *handle_decl_lst (list_t *lst);
static absyn_exn_handler_t *handle_exn_handler (absyn_exn_handler_t *node);
static list_t *handle_exn_lst (list_t *lst);
static absyn_exn_expr_t *handle_exn_expr (absyn_exn_expr_t *node);
static absyn_expr_t *handle_expr (absyn_expr_t *node);
static list_t *handle_expr_lst (list_t *lst);
static absyn_fun_call_t *handle_fun_call (absyn_fun_call_t *node);
static absyn_fun_decl_t *handle_fun_decl (absyn_fun_decl_t *node);
static absyn_if_expr_t *handle_if_expr (absyn_if_expr_t *node);
static absyn_module_decl_t *handle_module_decl (absyn_module_decl_t *node);
static list_t *handle_record_assn (list_t *lst);
static absyn_record_ref_t *handle_record_ref (absyn_record_ref_t *node);
static absyn_val_decl_t *handle_val_decl (absyn_val_decl_t *node);

/* Entry point for this pass. */
ast_t *lift_functions (ast_t *ast)
{
   list_t *tmp;

   for (tmp = ast; tmp != NULL; tmp = tmp->next)
      tmp->data = handle_module_decl (tmp->data);

   return ast;
}

/* +================================================================+
 * | SIMPLIFICATION FUNCTIONS - ONE PER (MOST) AST NODE TYPES       |
 * +================================================================+
 */

static absyn_decl_expr_t *handle_decl_expr (absyn_decl_expr_t *node)
{
   node->decl_lst = handle_decl_lst (node->decl_lst);
   node->expr = handle_expr (node->expr);
   return node;
}

static list_t *handle_decl_lst (list_t *lst)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_decl_t *decl = tmp->data;

      switch (decl->type) {
         case ABSYN_FUN_DECL:
            decl->fun_decl = handle_fun_decl (decl->fun_decl);
            break;

         case ABSYN_MODULE_DECL:
            decl->module_decl = handle_module_decl (decl->module_decl);
            break;

         case ABSYN_TY_DECL:
            break;

         case ABSYN_VAL_DECL:
            decl->val_decl = handle_val_decl (decl->val_decl);
            break;

#ifndef NEW_GRAMMAR
         default:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad decl->type");
            exit(1);
#endif
      }
   }

   return lst;
}

static absyn_exn_handler_t *handle_exn_handler (absyn_exn_handler_t *node)
{
   node->handler_lst = handle_exn_lst (node->handler_lst);

   if (node->default_handler != NULL)
      node->default_handler->expr = handle_expr (node->default_handler->expr);

   return node;
}

static list_t *handle_exn_lst (list_t *lst)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_exn_lst_t *node = tmp->data;
      node->expr = handle_expr (node->expr);
   }

   return lst;
}

static absyn_exn_expr_t *handle_exn_expr (absyn_exn_expr_t *node)
{
   node->values = handle_record_assn (node->values);
   return node;
}

static absyn_expr_t *handle_expr (absyn_expr_t *node)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
      case ABSYN_BOTTOM:
      case ABSYN_INTEGER:
      case ABSYN_STRING:
         break;

      case ABSYN_DECL:
         node->decl_expr = handle_decl_expr (node->decl_expr);
         break;

      case ABSYN_EXN:
         node->exn_expr = handle_exn_expr (node->exn_expr);
         break;

      case ABSYN_EXPR_LST:
         node->expr_lst = handle_expr_lst (node->expr_lst);
         break;

      case ABSYN_FUN_CALL:
         node->fun_call_expr = handle_fun_call (node->fun_call_expr);
         break;

      case ABSYN_ID:
      {
         backlink_t *parent;
         symtab_t *symtab;

         /* We only need to check values to see if they're bound or not. */
         if (node->identifier->kind != SYM_VALUE)
            break;

         /* Values that refer to another module are always free. */
         if (node->identifier->sub != NULL)
         {
            absyn_id_expr_t *tmp;
            
            fprintf (stderr, "%d:%d free value:  \n", node->identifier->lineno,
                     node->identifier->column);

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

         /* If the value is not found in the innermost symtab, it's free. */
         parent = find_lexical_parent (node->parent);

         switch (parent->kind) {
            case LINK_DECL_EXPR:
               symtab = ((absyn_decl_expr_t *) parent->ptr)->symtab;
               break;

            case LINK_EXN_LST:
               symtab = ((absyn_exn_lst_t *) parent->ptr)->symtab;
               break;

            case LINK_FUN_DECL:
               symtab = ((absyn_fun_decl_t *) parent->ptr)->symtab;
               break;

            case LINK_MODULE_DECL:
               symtab = ((absyn_module_decl_t *) parent->ptr)->symtab;
               break;

            default:
               MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad parent->kind");
               exit(1);
         }

         if (table_lookup_entry (symtab, node->identifier->symbol,
                                 SYM_VALUE) == NULL)
            fprintf (stderr, "%d:%d free value:  %ls\n",
                     node->identifier->lineno, node->identifier->column,
                     node->identifier->symbol);

         break;
      }

      case ABSYN_IF:
         node->if_expr = handle_if_expr (node->if_expr);
         break;

      case ABSYN_RAISE:
         node->raise_expr = handle_expr (node->raise_expr);
         break;

      case ABSYN_RECORD_ASSN:
         node->record_assn_lst = handle_record_assn (node->record_assn_lst);
         break;

      case ABSYN_RECORD_REF:
         node->record_ref = handle_record_ref (node->record_ref);
         break;

      /* Running into a case expression is impossible, but this shuts up gcc. */
      case ABSYN_CASE:
#ifndef NEW_GRAMMAR
      default:
#endif
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad node->kind for expr");
         exit(1);
   }

   if (node->exn_handler != NULL)
      node->exn_handler = handle_exn_handler (node->exn_handler);

   return node;
}

static list_t *handle_expr_lst (list_t *lst)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
      tmp->data = handle_expr (tmp->data);

   return lst;
}

static absyn_fun_call_t *handle_fun_call (absyn_fun_call_t *node)
{
   list_t *tmp;

   for (tmp = node->arg_lst; tmp != NULL; tmp = tmp->next)
      tmp->data = handle_expr (tmp->data);

   return node;
}

static absyn_fun_decl_t *handle_fun_decl (absyn_fun_decl_t *node)
{
   node->body = handle_decl_expr (node->body);
   return node;
}

static absyn_if_expr_t *handle_if_expr (absyn_if_expr_t *node)
{
   node->test_expr = handle_expr (node->test_expr);
   node->then_expr = handle_expr (node->then_expr);
   node->else_expr = handle_expr (node->else_expr);
   return node;
}

static absyn_module_decl_t *handle_module_decl (absyn_module_decl_t *node)
{
   node->decl_lst = handle_decl_lst (node->decl_lst);
   return node;
}

static list_t *handle_record_assn (list_t *lst)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_record_assn_t *node = tmp->data;
      node->expr = handle_expr (node->expr);
   }

   return lst;
}

static absyn_record_ref_t *handle_record_ref (absyn_record_ref_t *node)
{
   if (node->rec->kind == ABSYN_FUN_CALL)
      handle_fun_call (node->rec->fun_call_expr);

   return node;
}

static absyn_val_decl_t *handle_val_decl (absyn_val_decl_t *node)
{
   node->init = handle_expr (node->init);
   return node;
}

/* vim: set tags=../tags: */
