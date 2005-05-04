/* Perform conversions on case-exprs found in the abstract syntax tree with
 * the goal of simplifying it.  These conversions consist of converting
 * case-exprs with only an else branch into an expr, and exploding all other
 * case-exprs into a chain of if-exprs.  After this phase, case-exprs are
 * eliminated from the AST.
 *
 * This is a good pass to come first.  At the very least, it needs to come
 * before decl-expr transformations since we will be making decl-exprs here.
 *
 * $Id: desugar_case.c,v 1.6 2005/05/04 02:17:24 chris Exp $
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
#include "semant.h"

static absyn_decl_expr_t *handle_case_expr (absyn_case_expr_t *node);
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

/* Wrap an expr in a branch-lst element. */
static absyn_branch_lst_t *expr_to_branch_lst (absyn_expr_t *in, backlink_t *bl)
{
   absyn_branch_lst_t *retval;

   MALLOC (retval, sizeof(absyn_branch_lst_t));

   retval->lineno = in->lineno;
   retval->column = in->column;
   retval->parent = bl;
   retval->branch = NULL;
   retval->expr = in;

   retval->expr->parent = make_bl (LINK_BRANCH_LST, retval);
   return retval;
}

/* Convert a generic expr into a val-decl. */
static absyn_val_decl_t *expr_to_val_decl (absyn_expr_t *in, backlink_t *bl)
{
   absyn_val_decl_t *retval;

   MALLOC (retval, sizeof(absyn_val_decl_t));

   retval->lineno = in->lineno;
   retval->column = in->lineno;
   retval->parent = bl;
   retval->ty = in->ty;
   retval->symbol = str_to_id_expr (make_unique_str (L"__val_decl"),
                                    in->lineno, in->column);
   retval->ty_decl = NULL;
   retval->init = in;

   /* Reparent the val's init-expr. */
   retval->init->parent = make_bl (LINK_VAL_DECL, retval);

   return retval;
}

/* Build the subtree for calling a comparison function with a list of arguments
 * to see if we should follow this then-expr branch or not.  This sure is a
 * lot of work for something that should be so simple.
 */
static absyn_expr_t *make_test_expr (absyn_val_decl_t *left,
                                     absyn_expr_t *right)
{
   absyn_expr_t *retval, *val_expr;
   absyn_fun_call_t *fun_call;
   absyn_id_expr_t *id;

   MALLOC (retval, sizeof (absyn_expr_t));
   MALLOC (fun_call, sizeof (absyn_fun_call_t));
   MALLOC (id, sizeof (absyn_id_expr_t));
   MALLOC (val_expr, sizeof (absyn_expr_t));

   retval->lineno = left->lineno;
   retval->column = left->column;
   retval->parent = NULL;
   retval->exn_handler = NULL;
   retval->kind = ABSYN_FUN_CALL;
   retval->ty = left->ty;
   retval->fun_call_expr = fun_call;

   fun_call->lineno = left->lineno;
   fun_call->column = left->column;
   fun_call->parent = make_bl (LINK_EXPR, retval);
   fun_call->ty = left->ty;
   fun_call->identifier = id;

   /* Now we have to make a reference to the val-decl so we can append it
    * to the list of function call arguments.  This val-decl is how we're
    * referring to the result of evaluating the case-expr's test-expr.
    */
   val_expr->lineno = left->lineno;
   val_expr->column = right->lineno;
   val_expr->parent = make_bl (LINK_FUN_CALL, retval->fun_call_expr);
   val_expr->exn_handler = NULL;
   val_expr->kind = ABSYN_ID;
   val_expr->ty = left->ty;

   MALLOC (val_expr->identifier, sizeof (absyn_id_expr_t));
   val_expr->identifier->lineno = left->lineno;
   val_expr->identifier->column = left->column;
   val_expr->identifier->parent = make_bl (LINK_EXPR, val_expr);
   val_expr->identifier->symbol = left->symbol->symbol;
   val_expr->identifier->label = left->symbol->label;
   val_expr->identifier->sub = NULL;
   
   /* First append the val-expr for the test result, then append the expr for
    * the branch-test.
    */
   fun_call->arg_lst = list_append (fun_call->arg_lst, val_expr);
   fun_call->arg_lst = list_append (fun_call->arg_lst, right);

   /* Finally, what's the name of the function we are calling? */
   id->lineno = left->lineno;
   id->column = left->lineno;
   id->parent = make_bl (LINK_FUN_CALL, retval->fun_call_expr);

   switch ((unalias(left->ty))->ty) {
      case TY_BOOLEAN:
         id->symbol = wcsdup (L"Boolean");
         id->label = wcsdup (L"Boolean");
         break;

      case TY_INTEGER:
         id->symbol = wcsdup (L"Integer");
         id->label = wcsdup (L"Integer");
         break;

      case TY_STRING:
         id->symbol = wcsdup (L"String");
         id->label = wcsdup (L"String");
         break;

      case TY_ALIAS:
      case TY_ANY:
      case TY_BOTTOM:
      case TY_EXN:
      case TY_LIST:
      case TY_RECORD:
#ifndef NEW_TYPES
      default:
#endif
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad type for left->ty");
         exit(1);
         break;
   }

   MALLOC (id->sub, sizeof(absyn_id_expr_t));
   id->sub->lineno = id->lineno;
   id->sub->column = id->column;
   id->sub->parent = make_bl (LINK_ID_EXPR, id);
   id->sub->symbol = wcsdup (L"=");
   id->sub->label = wcsdup (L"=");
   id->sub->sub = NULL;

   return retval;
}

static absyn_expr_t *build_if_expr (absyn_val_decl_t *val, list_t *lst)
{
   absyn_expr_t *retval;
   absyn_if_expr_t *if_expr;
   absyn_branch_lst_t *branch = (absyn_branch_lst_t *) lst->data;
   backlink_t *bl;

   MALLOC (retval, sizeof(absyn_expr_t));
   MALLOC (if_expr, sizeof(absyn_if_expr_t));

   /* The calling function will need to set the parent link. */
   retval->lineno = val->lineno;
   retval->column = val->column;
   retval->parent = NULL;
   retval->exn_handler = NULL;
   retval->ty = val->ty;
   retval->kind = ABSYN_IF;

   bl = make_bl (LINK_IF_EXPR, if_expr);

   if_expr->lineno = val->lineno;
   if_expr->column = val->column;
   if_expr->parent = make_bl (LINK_EXPR, retval);
   if_expr->ty = val->ty;

   if_expr->test_expr = make_test_expr (val, branch->branch);
   if_expr->test_expr->parent = bl;
   if_expr->then_expr = handle_expr (branch->expr);
   if_expr->then_expr->parent = bl;

   /* If this is the last element in the branch-lst, it's the default and
    * we don't need to make this if-expr any more complicated.  However if
    * there are other elements, we have to recurse.
    */
   if (lst->next->next == NULL)
   {
      if_expr->else_expr =
         handle_expr(((absyn_branch_lst_t *) lst->next->data)->expr);
      if_expr->else_expr->parent = bl;
   }
   else
      if_expr->else_expr = build_if_expr (val, lst->next);

   retval->if_expr = if_expr;
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
   test_decl->val_decl = expr_to_val_decl (handle_expr(node->test),
                                           make_bl (LINK_DECL, test_decl));

   retval->decl_lst = list_append (retval->decl_lst, test_decl);

   /* Always try to append a possible default_expr to the branch_lst.  Then
    * if the branch_lst is only one element long, we can use that as the body
    * of the decl-expr.  Otherwise, we need to construct the chained if-exprs.
    */
   node->branch_lst = list_append (node->branch_lst,
                                   expr_to_branch_lst(node->default_expr, bl));

   if (list_length (node->branch_lst) == 1)
   {
      absyn_branch_lst_t *branch = node->branch_lst->data;

      retval->expr = handle_expr (branch->expr);
      retval->expr->parent = bl;
   }
   else
   {
      retval->expr = build_if_expr(test_decl->val_decl, node->branch_lst);
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
      node->expr = handle_expr(node->expr);
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

      case ABSYN_EXN:
         node->exn_expr = handle_exn_expr (node->exn_expr);
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

      case ABSYN_RAISE:
         node->raise_expr = handle_expr (node->raise_expr);
         break;

      case ABSYN_RECORD_ASSN:
         node->record_assn_lst = handle_record_assn (node->record_assn_lst);
         break;

      case ABSYN_RECORD_REF:
         node->record_ref = handle_record_ref (node->record_ref);
         break;

#ifndef NEW_GRAMMAR
         default:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad node->kind");
            exit(1);
#endif
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
