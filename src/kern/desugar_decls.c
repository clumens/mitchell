/* Perform conversions on decl-exprs found in the abstract syntax tree with
 * the goal of simplifying it.  These conversions include promoting certain
 * decl-exprs to functions thereby flattening out some of the nested structure,
 * and raising expressions out of decl-exprs with no decls.
 *
 * This pass must come after any phases that generate decl-exprs, but before
 * lambda lifting since we count on that to sort out the arguments to the
 * functions generated in promotion.
 *
 * $Id: desugar_decls.c,v 1.3 2005/02/12 16:26:19 chris Exp $
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

/* Processing a decl-expr can return two different types - a decl-expr in the
 * case where the original was the RHS of a function, and a fun-decl in all
 * other cases.  This is a special type for handling these two cases.
 */
typedef struct {
   enum { RETURN_DECL_EXPR, RETURN_FUN_DECL } kind;

   union {
      absyn_decl_expr_t *decl_expr;
      absyn_fun_decl_t  *fun;
   };
} decl_return_t;

static absyn_case_expr_t *handle_case_expr (absyn_case_expr_t *node);
static decl_return_t *handle_decl_expr (absyn_decl_expr_t *node,
                                        unsigned int promotable);
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
ast_t *desugar_decl_exprs (ast_t *ast)
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

/* Convert a decl-expr into a fun-decl.  The fun-decl may then be added into
 * the decl-lst of whatever currently holds the incoming decl-expr.
 */
static absyn_fun_decl_t *decl_expr_to_fun_decl (absyn_decl_expr_t *in)
{
   absyn_fun_decl_t *retval;

   MALLOC (retval, sizeof(absyn_fun_decl_t));
   MALLOC (retval->body, sizeof(absyn_expr_t));

   /* First create the shell of the new fun-decl, which will hold an expr
    * inside it.
    */
   retval->lineno = in->lineno;
   retval->column = in->column;
   retval->parent = in->parent;
   retval->symbol = make_unique_sym (L"__decl_expr", in->lineno, in->column);
   retval->formals = NULL;             /* will be fixed by lambda lifting */

   /* Now create the inner expr, which will hold a decl-expr. */
   retval->body->lineno = in->lineno;
   retval->body->column = in->column;
   retval->body->parent = in->parent;
   retval->body->kind = ABSYN_DECL;
   retval->body->ty = in->ty;
   retval->body->decl_expr = in;

   return retval;
}

/* Build a function call expression that can replace a promoted decl-expr. */
static absyn_expr_t *make_fun_call_expr (absyn_id_expr_t *in, backlink_t *p)
{
   absyn_expr_t *retval;

   MALLOC (retval, sizeof(absyn_expr_t));
   MALLOC (retval->fun_call_expr, sizeof(absyn_fun_call_t));

   retval->lineno = in->lineno;
   retval->column = in->column;
   retval->parent = p;
   retval->kind = ABSYN_FUN_CALL;
   retval->ty = NULL;

   retval->fun_call_expr->lineno = in->lineno;
   retval->fun_call_expr->column = in->column;
   retval->fun_call_expr->parent =
      make_bl (LINK_FUN_CALL, retval->fun_call_expr);
   retval->fun_call_expr->ty = NULL;
   retval->fun_call_expr->identifier = in;
   retval->fun_call_expr->arg_lst = NULL;

   return retval;
}

/* +================================================================+
 * | SIMPLIFICATION FUNCTIONS - ONE PER (MOST) AST NODE TYPES       |
 * +================================================================+
 */

/* XXX: This is only here until the case-expr phase is written.  Then we
 * won't have to deal with case-exprs anymore.  Hooray.
 */
static absyn_case_expr_t *handle_case_expr (absyn_case_expr_t *node)
{
   list_t *tmp;

   node->test = handle_expr (node->test);

   for (tmp = node->branch_lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_branch_lst_t *b = tmp->data;
      tmp->data = handle_expr (b->expr);
   }

   if (node->default_expr != NULL)
      node->default_expr = handle_expr (node->default_expr);

   return node;
}

/* Promote certain decl-exprs into functions.  This is done by creating a new
 * function inside the scope containing the decl-expr immediately before the
 * decl-expr, moving the contents of the decl-expr into that function, and then
 * replacing the decl-expr with a call to the new function.
 *
 * We do not promote decl-exprs which are the body of a function since those
 * are already a function and that would be dumb.
 */
static decl_return_t *handle_decl_expr (absyn_decl_expr_t *node,
                                        unsigned int promotable)
{
   decl_return_t *retval;

   MALLOC (retval, sizeof(decl_return_t));

   if (promotable)
   {
      MALLOC (retval->fun, sizeof(absyn_decl_t));
      retval->kind = RETURN_FUN_DECL;
      retval->fun = handle_fun_decl (decl_expr_to_fun_decl (node));
   }
   else
   {
      /* This decl-expr is not promotable because it's the RHS of a function
       * so there'd be no point in promoting.  Therefore, recurse into it
       * just like any other random AST blob.
       */
      node->decl_lst = handle_decl_lst (node->decl_lst);
      node->expr = handle_expr (node->expr);

      retval->kind = RETURN_DECL_EXPR;
      retval->decl_expr = node;
   }

   return retval;
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

      case ABSYN_CASE:
         node->case_expr = handle_case_expr (node->case_expr);
         break;

      case ABSYN_DECL:
      {
         absyn_decl_t *decl;
         absyn_fun_decl_t *new_fun =
            (handle_decl_expr (node->decl_expr, 1))->fun;
         backlink_t *parent = find_lexical_parent (node->parent);

         MALLOC(decl, sizeof(absyn_decl_t));
         decl->lineno = new_fun->lineno;
         decl->column = new_fun->column;
         decl->type = ABSYN_FUN_DECL;
         decl->fun_decl = new_fun;

         new_fun->parent = make_bl (LINK_DECL, decl);

         /* We only have two possibilities here, because we eliminated the
          * third in the parser by ensuring all functions have a decl-expr
          * as their body.  We'll strip out empty decl-exprs later.
          */
         switch (parent->kind) {
            case LINK_DECL_EXPR:
            {
               absyn_decl_expr_t *p = (absyn_decl_expr_t *) parent->ptr;

               decl->parent = make_bl (LINK_DECL_EXPR, p);
               p->decl_lst = list_append (p->decl_lst, decl);
               return make_fun_call_expr (new_fun->symbol,
                                          make_bl (LINK_DECL, decl));
            }

            case LINK_MODULE_DECL:
            {
               absyn_module_decl_t *p = (absyn_module_decl_t *) parent->ptr;

               decl->parent = make_bl (LINK_MODULE_DECL, p);
               p->decl_lst = list_append (p->decl_lst, decl);
               return make_fun_call_expr (new_fun->symbol,
                                          make_bl (LINK_DECL, decl));
            }

            default:
               MITCHELL_INTERNAL_ERROR (cconfig.filename,
                                        "bad parent->kind for expr");
               exit(1);
         }

         break;
      }

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
   if (node->body->kind == ABSYN_DECL)
   {
      decl_return_t *decl = handle_decl_expr (node->body->decl_expr, 0);
      node->body->decl_expr = decl->decl_expr;
   }
   else
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
