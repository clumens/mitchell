/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.1 2004/11/14 17:16:53 chris Exp $
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
#include <wchar.h>

#include "absyn.h"
#include "memory.h"
#include "symtab.h"

/* The global symbol table stack. */
static tabstack_t *global = NULL;

/* More mutually recursive functions for yet another tree walk. */
static void check_branch_lst (absyn_branch_lst_t *node);
static void check_case_expr (absyn_case_expr_t *node);
static void check_decl (absyn_decl_t *node);
static void check_decl_expr (absyn_decl_expr_t *node);
static void check_decl_lst (absyn_decl_lst_t *node);
static void check_expr (absyn_expr_t *node);
static void check_expr_lst (absyn_expr_lst_t *node);
static void check_fun_decl (absyn_fun_decl_t *node);
static symbol_t *check_id (absyn_id_expr_t *node);
static void check_id_lst (absyn_id_lst_t *node);
static void check_if_expr (absyn_if_expr_t *node);
static void check_module_decl (absyn_module_decl_t *node);
static void check_module_lst (absyn_module_lst_t *node);
static void check_record_lst (absyn_record_lst_t *node);
static void check_ty (absyn_ty_t *node);
static void check_ty_decl (absyn_ty_decl_t *node);
static void check_val_decl (absyn_val_decl_t *node);

/* Semantic analysis entry point. */
void check_program (ast_t *ast)
{
   global = enter_scope (global);
   check_module_lst (ast);
   global = leave_scope (global);
}

/* Check for duplicate branches. */
static void check_branch_lst (absyn_branch_lst_t *node)
{
   absyn_branch_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_expr (tmp->branch);
      check_expr (tmp->expr);
      tmp = tmp->next;
   }
}

static void check_case_expr (absyn_case_expr_t *node)
{
   check_expr (node->test);
   check_branch_lst (node->branch_lst);
   if (node->default_expr != NULL)
      check_expr (node->default_expr);
}

static void check_decl (absyn_decl_t *node)
{
   switch (node->type) {
      case ABSYN_FUN_DECL:
         check_fun_decl (node->fun_decl);
         break;

      case ABSYN_MODULE_DECL:
         check_module_decl (node->module_decl);
         break;

      case ABSYN_TY_DECL:
         check_ty_decl (node->ty_decl);
         break;

      case ABSYN_VAL_DECL:
         check_val_decl (node->val_decl);
         break;
   }
}

static void check_decl_expr (absyn_decl_expr_t *node)
{
   global = enter_scope (global);
   check_decl_lst (node->decl_lst);
   check_expr (node->expr);
   global = leave_scope (global);
}

static void check_decl_lst (absyn_decl_lst_t *node)
{
   absyn_decl_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_decl (tmp->decl);
      tmp = tmp->next;
   }
}

static void check_expr (absyn_expr_t *node)
{
   switch (node->type) {
      case ABSYN_BOOLEAN:
         break;

      case ABSYN_CASE:
         check_case_expr (node->case_expr);
         break;

      case ABSYN_DECL:
         check_decl_expr (node->decl_expr);
         break;

      case ABSYN_EXPR_LST:
         check_expr_lst (node->expr_lst);
         break;

      case ABSYN_FUN_CALL:
         check_id (node->fun_call_expr.identifier);
         check_expr_lst (node->fun_call_expr.arg_lst);
         break;

      case ABSYN_ID:
         {
            symbol_t *s = check_id (node->identifier);
            s->kind = SYM_FUNVAL;

            if (!symtab_entry_exists (global, s))
            {
               fprintf (stderr, "referenced unknown symbol: %ls\n",
                                (wchar_t *) s->name);
               fprintf (stderr, "exiting.\n");
               exit (1);
            }
            break;
         }

      case ABSYN_IF:
         check_if_expr (node->if_expr);
         break;

      case ABSYN_INTEGER:
         break;

      case ABSYN_RECORD_LST:
         check_record_lst (node->record_assn_lst);
         break;

      case ABSYN_STRING:
         break;
   }
}

static void check_expr_lst (absyn_expr_lst_t *node)
{
   absyn_expr_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_expr (tmp->expr);
      tmp = tmp->next;
   }
}

static void check_fun_decl (absyn_fun_decl_t *node)
{
   /* The function name needs to be entered into the outer scope since it is
    * able to be referenced from out there.
    */
   check_id (node->symbol);
   check_ty (node->ty);
   
   /* However, the parameters are only meaningful inside the function itself,
    * so enter a new level of scope before entering those.
    */
   global = enter_scope (global);
   check_id_lst (node->id_lst);
   check_expr (node->body);
   global = leave_scope (global);
}

static symbol_t *check_id (absyn_id_expr_t *node)
{
   symbol_t *retval = NULL;

   if (node->ns == NULL)
   {
      MALLOC (retval, sizeof (symbol_t))
      retval->name = (mstring_t *) wcsdup (node->symbol);
   }
   else
      check_id (node->ns);

   return retval;
}

/* Check for duplicate names. */
static void check_id_lst (absyn_id_lst_t *node)
{
   absyn_id_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_id (tmp->symbol);
      check_ty (tmp->ty);
      tmp = tmp->next;
   }
}

static void check_if_expr (absyn_if_expr_t *node)
{
   check_expr (node->test_expr);
   check_expr (node->then_expr);
   check_expr (node->else_expr);
}

static void check_module_decl (absyn_module_decl_t *node)
{
   symbol_t *s = check_id (node->symbol);

   s->kind = SYM_MODULE;
   symtab_add_entry (global, s);

   global = enter_scope (global);
   check_decl_lst (node->decl_lst);
   global = leave_scope (global);
}

static void check_module_lst (absyn_module_lst_t *node)
{
   absyn_module_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_module_decl (tmp->module);
      tmp = tmp->next;
   }
}

static void check_record_lst (absyn_record_lst_t *node)
{
   absyn_record_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_id (tmp->symbol);
      check_expr (tmp->expr);
      tmp = tmp->next;
   }
}

static void check_ty (absyn_ty_t *node)
{
   /* something about is_list */
   if (node->is_record)
      check_id_lst (node->record);
   else
      check_id (node->identifier);
}

static void check_ty_decl (absyn_ty_decl_t *node)
{
   symbol_t *s = check_id (node->symbol);

   s->kind = SYM_TYPE;
   symtab_add_entry (global, s);

   check_ty (node->ty);
}

static void check_val_decl (absyn_val_decl_t *node)
{
   symbol_t *s = check_id (node->symbol);

   s->kind = SYM_FUNVAL;
   symtab_add_entry (global, s);

   check_ty (node->ty);
   check_expr (node->init);
}

/* vim: set tags=../tags: */
