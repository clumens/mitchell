/* Generic abstract syntax tree walking functions.  These functions perform
 * only the most basic of functions by walking the AST and visiting each node.
 * Override these basic versions with more complicated ones if that's what
 * a certain pass requires.
 *  
 * $Id: absyn_walk.c,v 1.6 2005/08/04 03:21:02 chris Exp $
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
#include "error.h"
#include "list.h"
#include "memory.h"
#include "translate.h"

/* Set up an initial set of AST walking function pointers.  After calling this
 * function, individual passes are free to override any of the pointers with
 * their own versions of the functions.
 */
absyn_funcs_t *init_default_funcs()
{
   absyn_funcs_t *retval;
   
   MALLOC(retval, sizeof(absyn_funcs_t));
   retval->visit_decl_expr = visit_decl_expr;
   retval->visit_decl_lst = visit_decl_lst;
   retval->visit_exn_handler = visit_exn_handler;
   retval->visit_exn_lst = visit_exn_lst;
   retval->visit_exn_expr = visit_exn_expr;
   retval->visit_expr = visit_expr;
   retval->visit_expr_lst = visit_expr_lst;
   retval->visit_fun_call = visit_fun_call;
   retval->visit_fun_decl = visit_fun_decl;
   retval->visit_if_expr = visit_if_expr;
   retval->visit_module_decl = visit_module_decl;
   retval->visit_record_assn = visit_record_assn;
   retval->visit_record_ref = visit_record_ref;
   retval->visit_val_decl = visit_val_decl;

   return retval;
}

/* +================================================================+
 * | GENERIC AST WALKING FUNCTIONS                                  |
 * +================================================================+
 */

absyn_decl_expr_t *visit_decl_expr (absyn_funcs_t *funcs, absyn_decl_expr_t *node, void **user_data)
{
   node->decl_lst = funcs->visit_decl_lst (funcs, node->decl_lst, user_data);
   node->expr = funcs->visit_expr (funcs, node->expr, user_data);
   return node;
}

list_t *visit_decl_lst (absyn_funcs_t *funcs, list_t *lst, void **user_data)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_decl_t *decl = tmp->data;

      switch (decl->type) {
         case ABSYN_FUN_DECL:
            decl->fun_decl = funcs->visit_fun_decl (funcs, decl->fun_decl, user_data);
            break;

         case ABSYN_MODULE_DECL:
            decl->module_decl = funcs->visit_module_decl (funcs, decl->module_decl, user_data);
            break;

         case ABSYN_TY_DECL:
            break;

         case ABSYN_VAL_DECL:
            decl->val_decl = funcs->visit_val_decl (funcs, decl->val_decl, user_data);
            break;

#ifndef NEW_GRAMMAR
         default:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__, N_("New AST type node type not handled.\n"));
#endif
      }
   }

   return lst;
}

absyn_exn_handler_t *visit_exn_handler (absyn_funcs_t *funcs, absyn_exn_handler_t *node, void **user_data)
{
   node->handler_lst = funcs->visit_exn_lst (funcs, node->handler_lst, user_data);

   if (node->default_handler != NULL)
      node->default_handler->expr = funcs->visit_expr (funcs, node->default_handler->expr, user_data);

   return node;
}

list_t *visit_exn_lst (absyn_funcs_t *funcs, list_t *lst, void **user_data)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_exn_lst_t *node = tmp->data;
      node->expr = funcs->visit_expr (funcs, node->expr, user_data);
   }

   return lst;
}

absyn_exn_expr_t *visit_exn_expr (absyn_funcs_t *funcs, absyn_exn_expr_t *node, void **user_data)
{
   node->values = funcs->visit_record_assn (funcs, node->values, user_data);
   return node;
}

absyn_expr_t *visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node, void **user_data)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
      case ABSYN_BOTTOM:
      case ABSYN_ID:
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

list_t *visit_expr_lst (absyn_funcs_t *funcs, list_t *lst, void **user_data)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
      tmp->data = funcs->visit_expr (funcs, tmp->data, user_data);

   return lst;
}

absyn_fun_call_t *visit_fun_call (absyn_funcs_t *funcs, absyn_fun_call_t *node, void **user_data)
{
   list_t *tmp;

   for (tmp = node->arg_lst; tmp != NULL; tmp = tmp->next)
      tmp->data = funcs->visit_expr (funcs, tmp->data, user_data);

   return node;
}

absyn_fun_decl_t *visit_fun_decl (absyn_funcs_t *funcs, absyn_fun_decl_t *node, void **user_data)
{
   node->body = funcs->visit_decl_expr (funcs, node->body, user_data);
   return node;
}

absyn_if_expr_t *visit_if_expr (absyn_funcs_t *funcs, absyn_if_expr_t *node, void **user_data)
{
   node->test_expr = funcs->visit_expr (funcs, node->test_expr, user_data);
   node->then_expr = funcs->visit_expr (funcs, node->then_expr, user_data);
   node->else_expr = funcs->visit_expr (funcs, node->else_expr, user_data);
   return node;
}

absyn_module_decl_t *visit_module_decl (absyn_funcs_t *funcs, absyn_module_decl_t *node, void **user_data)
{
   node->decl_lst = funcs->visit_decl_lst (funcs, node->decl_lst, user_data);
   return node;
}

list_t *visit_record_assn (absyn_funcs_t *funcs, list_t *lst, void **user_data)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_record_assn_t *node = tmp->data;
      node->expr = funcs->visit_expr (funcs, node->expr, user_data);
   }

   return lst;
}

absyn_record_ref_t *visit_record_ref (absyn_funcs_t *funcs, absyn_record_ref_t *node, void **user_data)
{
   funcs->visit_expr (funcs, node->rec, user_data);
   return node;
}

absyn_val_decl_t *visit_val_decl (absyn_funcs_t *funcs, absyn_val_decl_t *node, void **user_data)
{
   node->init = funcs->visit_expr (funcs, node->init, user_data);
   return node;
}

/* vim: set tags=../tags: */
