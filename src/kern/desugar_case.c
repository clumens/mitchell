/* Perform conversions on case-exprs found in the abstract syntax tree with
 * the goal of simplifying it.  These conversions consist of converting
 * case-exprs with only an else branch into an expr, and exploding all other
 * case-exprs into a chain of if-exprs.  After this phase, case-exprs are
 * eliminated from the AST.
 *
 * This is a good pass to come first.  At the very least, it needs to come
 * before decl-expr transformations since we will be making decl-exprs here.
 *
 * $Id: desugar_case.c,v 1.1 2005/02/12 16:26:19 chris Exp $
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

#include "absyn.h"
#include "basic_types.h"
#include "desugar.h"
#include "error.h"
#include "list.h"
#include "memory.h"
#include "str.h"

static absyn_decl_expr_t *handle_case_expr (absyn_case_expr_t *node);
static absyn_decl_expr_t *handle_decl_expr (absyn_decl_expr_t *node);
static list_t *handle_decl_lst (list_t *lst);
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
ast_t *desugar_case_exprs (ast_t *ast)
{
   list_t *tmp;

   for (tmp = ast; tmp != NULL; tmp = tmp->next)
      tmp->data = handle_module_decl (tmp->data);

   return ast;
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

/* Convert a generic expr into a val-decl. */
static absyn_val_decl_t *expr_to_val_decl (absyn_expr_t *in, backlink_t *bl)
{
   absyn_val_decl_t *retval;

   MALLOC (retval, sizeof(absyn_val_decl_t));

   retval->lineno = in->lineno;
   retval->column = in->lineno;
   retval->parent = bl;
   retval->ty = in->ty;
   retval->symbol = make_unique_sym (L"__val_decl", in->lineno, in->column);
   retval->ty_decl = NULL;
   retval->init = in;

   /* Reparent the val's init-expr. */
   retval->init->parent = make_bl (LINK_VAL_DECL, retval);

   return retval;
}

/* +================================================================+
 * | SIMPLIFICATION FUNCTIONS - ONE PER (MOST) AST NODE TYPES       |
 * +================================================================+
 */
static absyn_decl_expr_t *handle_case_expr (absyn_case_expr_t *node)
{
   absyn_decl_expr_t *retval;
   absyn_decl_t *test_decl;
   backlink_t *bl;

   MALLOC (retval, sizeof(absyn_decl_expr_t));
   MALLOC (test_decl, sizeof(absyn_decl_t));

   /* First, make the decl-expr that will hold everything else. */
   retval->lineno = node->lineno;
   retval->column = node->column;
   retval->parent = node->parent;
   retval->ty = node->ty;

   bl = make_bl (LINK_DECL_EXPR, retval);

   /* Now construct a decl to hold the result of evaluating the test-expr.
    * We need this even if the case-expr only has a default branch.
    */
   test_decl->lineno = node->test->lineno;
   test_decl->column = node->test->column;
   test_decl->parent = bl;
   test_decl->type = ABSYN_VAL_DECL;
   test_decl->val_decl = expr_to_val_decl (node->test,
                                           make_bl (LINK_DECL, test_decl));

   retval->decl_lst = list_append (retval->decl_lst, test_decl);

   if (node->branch_lst == NULL && node->default_expr != NULL)
   {
      retval->expr = node->default_expr;
      retval->expr->parent = bl;
   }

   return retval;
}

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
      }
   }

   return lst;
}

static absyn_expr_t *handle_expr (absyn_expr_t *node)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
      case ABSYN_BOTTOM:
      case ABSYN_ID:
      case ABSYN_INTEGER:
      case ABSYN_STRING:
         break;

      /* Convert case-exprs into decl-exprs, then recurse over those. */
      case ABSYN_CASE:
         node->kind = ABSYN_DECL;
         node->decl_expr = handle_decl_expr(handle_case_expr (node->case_expr));
         break;

      case ABSYN_DECL:
         node->decl_expr = handle_decl_expr (node->decl_expr);
         break;

      case ABSYN_EXPR_LST:
         node->expr_lst = handle_expr_lst (node->expr_lst);
         break;

      case ABSYN_FUN_CALL:
         node->fun_call_expr = handle_fun_call (node->fun_call_expr);
         break;

      case ABSYN_IF:
         node->if_expr = handle_if_expr (node->if_expr);
         break;

      case ABSYN_RECORD_ASSN:
         node->record_assn_lst = handle_record_assn (node->record_assn_lst);
         break;

      case ABSYN_RECORD_REF:
         node->record_ref = handle_record_ref (node->record_ref);
         break;
   }

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
   node->body = handle_expr (node->body);
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
      tmp->data = handle_expr (node->expr);
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
