/* Perform conversions on decl-exprs found in the abstract syntax tree with
 * the goal of simplifying it.  These conversions include promoting certain
 * decl-exprs to functions thereby flattening out some of the nested structure,
 * and raising expressions out of decl-exprs with no decls.
 *
 * This pass must come after any phases that generate decl-exprs, but before
 * lambda lifting since we count on that to sort out the arguments to the
 * functions generated in promotion.
 *
 * $Id: desugar_decls.c,v 1.10 2005/06/30 12:52:56 chris Exp $
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
#include "absyn_walk.h"
#include "basic_types.h"
#include "desugar.h"
#include "error.h"
#include "list.h"
#include "memory.h"
#include "str.h"
#include "symtab.h"

static absyn_expr_t *decl_visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node);

/* Entry point for this pass. */
ast_t *desugar_decl_exprs (absyn_funcs_t *funcs, ast_t *ast)
{
   list_t *tmp;

   for (tmp = ast; tmp != NULL; tmp = tmp->next)
      tmp->data = funcs->visit_module_decl (funcs, tmp->data);

   return ast;
}

/* Initialization for this pass. */
absyn_funcs_t *init_decl_pass()
{
   absyn_funcs_t *retval = init_default_funcs();
   retval->visit_expr = decl_visit_expr;
   return retval;
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

   /* First create the shell of the new fun-decl, which will hold an expr
    * inside it.
    */
   retval->lineno = in->lineno;
   retval->column = in->column;
   retval->parent = in->parent;
   retval->symbol = str_to_id_expr (make_unique_str (L"__decl_expr"),
                                    in->lineno, in->column);
   retval->formals = NULL;             /* will be fixed by lambda lifting */

   /* Create a new symbol table for the new function.  We won't be doing
    * any type checking with this table, but it'll be handy in free variable
    * analysis later.
    */
   retval->symtab = symtab_new();

   /* Now link in the decl-expr as the function's body, making sure to reparent
    * it.
    */
   retval->body = in;
   in->parent = make_bl (LINK_FUN_DECL, retval);

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
   retval->exn_handler = NULL;
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
 * | PASS-LOCAL AST WALKING FUNCTIONS                               |
 * +================================================================+
 */

static absyn_expr_t *decl_visit_expr (absyn_funcs_t *funcs, absyn_expr_t *node)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
      case ABSYN_BOTTOM:
      case ABSYN_ID:
      case ABSYN_INTEGER:
      case ABSYN_STRING:
         break;

      case ABSYN_DECL:
      {
         absyn_decl_t *decl;
         absyn_decl_expr_t *decl_expr =
            funcs->visit_decl_expr (funcs, node->decl_expr);
         absyn_fun_decl_t *new_fun = decl_expr_to_fun_decl (decl_expr);
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
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad node->kind for expr");
         exit(1);
   }

   if (node->exn_handler != NULL)
      node->exn_handler = funcs->visit_exn_handler (funcs, node->exn_handler);

   return node;
}

/* vim: set tags=../tags: */
