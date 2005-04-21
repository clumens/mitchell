/* Rename all function and value identifiers to satisfy certain uniqueness
 * constraints in preparation for lambda lifting.  This pass also converts
 * symbols into a form that an assembler can use.  The following manipulations
 * need to be performed:
 *
 * - Uniquely name all functions within a module.  This is because we are
 *   raising all functions to the top-level module scope, so they will all
 *   be living in the same scope.  Values do not need this treatment.  Watch
 *   out that a lifted function does not have the same name as a value.
 * - Convert all symbol names into the character set allowed by the assembler.
 *   This conversion must be one-to-one, as we may need to convert back.
 *
 * The original symbol names must be preserved in the symbol table.
 *
 * This pass must come after any passes that create new functions or values,
 * but before lambda lifting.
 *
 * $Id: rename.c,v 1.3 2005/04/20 22:51:59 chris Exp $
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
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "absyn.h"
#include "basic_types.h"
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
static absyn_id_expr_t *handle_identifier_def (absyn_id_expr_t *node);
static absyn_id_expr_t *handle_identifier_use (absyn_id_expr_t *node);
static absyn_if_expr_t *handle_if_expr (absyn_if_expr_t *node);
static absyn_module_decl_t *handle_module_decl (absyn_module_decl_t *node);
static list_t *handle_record_assn (list_t *lst);
static absyn_record_ref_t *handle_record_ref (absyn_record_ref_t *node);
static absyn_val_decl_t *handle_val_decl (absyn_val_decl_t *node);

/* Entry point for this pass. */
ast_t *rename_identifiers (ast_t *ast)
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
      node->exn_id = handle_identifier_use (node->exn_id);
      node->expr = handle_expr(node->expr);
   }

   return tmp;
}

static absyn_exn_expr_t *handle_exn_expr (absyn_exn_expr_t *node)
{
   node->identifier = handle_identifier_use (node->identifier);
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
         node->identifier = handle_identifier_use (node->identifier);
         break;

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

   node->identifier = handle_identifier_use (node->identifier);

   for (tmp = node->arg_lst; tmp != NULL; tmp = tmp->next)
      tmp->data = handle_expr (tmp->data);

   return node;
}

static absyn_fun_decl_t *handle_fun_decl (absyn_fun_decl_t *node)
{
   node->symbol = handle_identifier_def (node->symbol);
   node->body = handle_expr (node->body);
   return node;
}

static absyn_id_expr_t *handle_identifier_def (absyn_id_expr_t *node)
{
   return node;
}

static absyn_id_expr_t *handle_identifier_use (absyn_id_expr_t *node)
{
   absyn_id_expr_t *tmp;

   for (tmp = node; tmp != NULL; tmp = tmp->sub)
      tmp->symbol = (mstring_t *) tmp->label;

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
   node->rec = handle_expr(node->rec);
   return node;
}

static absyn_val_decl_t *handle_val_decl (absyn_val_decl_t *node)
{
   node->symbol = handle_identifier_def (node->symbol);
   node->init = handle_expr (node->init);
   return node;
}

/* vim: set tags=../tags: */
