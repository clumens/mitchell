/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.3 2004/11/20 19:44:05 chris Exp $
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

/* This is the base environment, containing all the predefined values,
 * functions, modules, and types.  Keep this as absolutely small as possible.
 */
static symbol_t base_env[] = {
   { SYM_FUNVAL, (mstring_t *) L"f" },
   { SYM_FUNVAL, (mstring_t *) L"t" },
   { SYM_TYPE, (mstring_t *) L"⊥" },
   { SYM_TYPE, (mstring_t *) L"boolean" },
   { SYM_TYPE, (mstring_t *) L"integer" },
   { SYM_TYPE, (mstring_t *) L"string" },
   { SYM_TYPE, NULL }
};

/* The global symbol table stack. */
static tabstack_t *global = NULL;

/* More mutually recursive functions for yet another tree walk. */
static void check_branch_lst (absyn_branch_lst_t *node, tabstack_t *stack);
static void check_case_expr (absyn_case_expr_t *node, tabstack_t *stack);
static void check_decl (absyn_decl_t *node, tabstack_t *stack);
static void check_decl_expr (absyn_decl_expr_t *node, tabstack_t *stack);
static void check_decl_lst (absyn_decl_lst_t *node, tabstack_t *stack);
static void check_expr (absyn_expr_t *node, tabstack_t *stack);
static void check_expr_lst (absyn_expr_lst_t *node, tabstack_t *stack);
static void check_fun_decl (absyn_fun_decl_t *node, tabstack_t *stack);
static symbol_t *check_id (absyn_id_expr_t *node, tabstack_t *stack);
static void check_id_lst (absyn_id_lst_t *node, tabstack_t *stack);
static void check_if_expr (absyn_if_expr_t *node, tabstack_t *stack);
static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack);
static void check_module_lst (absyn_module_lst_t *node, tabstack_t *stack);
static void check_record_lst (absyn_record_lst_t *node, tabstack_t *stack);
static void check_ty (absyn_ty_t *node, tabstack_t *stack);
static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack);
static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack);

static void populate_base_env (tabstack_t *stack)
{
   unsigned int i;

   for (i = 0; base_env[i].name != NULL; i++)
      symtab_add_entry (stack, &base_env[i]);
}

/* Semantic analysis entry point. */
void check_program (ast_t *ast)
{
   global = enter_scope (global);
   populate_base_env(global);
   check_module_lst (ast, global);
   global = leave_scope (global);
}

static void duplicate_symbol_error (symbol_t *s, unsigned int lineno,
                                    tabstack_t *stack)
{
   fprintf (stderr, "*** duplicate symbol referenced on line %d:  %ls\n",
                    lineno, (wchar_t *) s->name);
   symtab_dump (stack);
   exit (1);
}

static void unknown_symbol_error (symbol_t *s, unsigned int lineno,
                                  tabstack_t *stack)
{
   fprintf (stderr, "*** unknown symbol referenced on line %d:  %ls\n",
                    lineno, (wchar_t *) s->name);
   symtab_dump (stack);
   exit (1);
}

/* +================================================================+
 * | TYPE CHECKING FUNCTIONS - ONE PER AST NODE TYPE                |
 * +================================================================+
 */

/* Check for duplicate branches. */
static void check_branch_lst (absyn_branch_lst_t *node, tabstack_t *stack)
{
   absyn_branch_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_expr (tmp->branch, stack);
      check_expr (tmp->expr, stack);
      tmp = tmp->next;
   }
}

static void check_case_expr (absyn_case_expr_t *node, tabstack_t *stack)
{
   check_expr (node->test, stack);
   check_branch_lst (node->branch_lst, stack);

   if (node->default_expr != NULL)
      check_expr (node->default_expr, stack);
}

static void check_decl (absyn_decl_t *node, tabstack_t *stack)
{
   switch (node->type) {
      case ABSYN_FUN_DECL:
         check_fun_decl (node->fun_decl, stack);
         break;

      case ABSYN_MODULE_DECL:
         check_module_decl (node->module_decl, stack);
         break;

      case ABSYN_TY_DECL:
         check_ty_decl (node->ty_decl, stack);
         break;

      case ABSYN_VAL_DECL:
         check_val_decl (node->val_decl, stack);
         break;
   }
}

static void check_decl_expr (absyn_decl_expr_t *node, tabstack_t *stack)
{
   stack = enter_scope (stack);
   check_decl_lst (node->decl_lst, stack);
   check_expr (node->expr, stack);
   stack = leave_scope (stack);
}

static void check_decl_lst (absyn_decl_lst_t *node, tabstack_t *stack)
{
   absyn_decl_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_decl (tmp->decl, stack);
      tmp = tmp->next;
   }
}

static void check_expr (absyn_expr_t *node, tabstack_t *stack)
{
   switch (node->type) {
      case ABSYN_BOOLEAN:
         break;

      case ABSYN_CASE:
         check_case_expr (node->case_expr, stack);
         break;

      case ABSYN_DECL:
         check_decl_expr (node->decl_expr, stack);
         break;

      case ABSYN_EXPR_LST:
         check_expr_lst (node->expr_lst, stack);
         break;

      case ABSYN_FUN_CALL:
         check_id (node->fun_call_expr.identifier, stack);
         check_expr_lst (node->fun_call_expr.arg_lst, stack);
         break;

      case ABSYN_ID:
         {
            symbol_t *s = check_id (node->identifier, stack);
            s->kind = SYM_FUNVAL;

            if (!symtab_entry_exists (stack, s))
               unknown_symbol_error (s, node->lineno, stack);
            break;
         }

      case ABSYN_IF:
         check_if_expr (node->if_expr, stack);
         break;

      case ABSYN_INTEGER:
         break;

      case ABSYN_RECORD_LST:
         check_record_lst (node->record_assn_lst, stack);
         break;

      case ABSYN_STRING:
         break;
   }
}

static void check_expr_lst (absyn_expr_lst_t *node, tabstack_t *stack)
{
   absyn_expr_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_expr (tmp->expr, stack);
      tmp = tmp->next;
   }
}

static void check_fun_decl (absyn_fun_decl_t *node, tabstack_t *stack)
{
   /* The function name needs to be entered into the outer scope since it is
    * able to be referenced from out there.
    */
   check_id (node->symbol, stack);
   check_ty (node->ty, stack);
   
   /* However, the parameters are only meaningful inside the function itself,
    * so enter a new level of scope before entering those.
    */
   stack = enter_scope (stack);
   check_id_lst (node->id_lst, stack);
   check_expr (node->body, stack);
   stack = leave_scope (stack);
}

static symbol_t *check_id (absyn_id_expr_t *node, tabstack_t *stack)
{
   symbol_t *retval = NULL;

   if (node->ns == NULL)
   {
      MALLOC (retval, sizeof (symbol_t))
      retval->name = (mstring_t *) wcsdup (node->symbol);
      retval->kind = SYM_FUNVAL;
      
      if (symtab_add_entry (stack, retval) == -1)
         duplicate_symbol_error (retval, node->lineno, stack);
   }
   else
      check_id (node->ns, stack);

   return retval;
}

/* Check for duplicate names. */
static void check_id_lst (absyn_id_lst_t *node, tabstack_t *stack)
{
   absyn_id_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_id (tmp->symbol, stack);
      check_ty (tmp->ty, stack);
      tmp = tmp->next;
   }
}

static void check_if_expr (absyn_if_expr_t *node, tabstack_t *stack)
{
   check_expr (node->test_expr, stack);
   check_expr (node->then_expr, stack);
   check_expr (node->else_expr, stack);
}

static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack)
{
   symbol_t s;

   if (node->symbol->ns != NULL)
   {
      fprintf (stderr, "*** error on line %d:  module name includes "
                       "namespace:  %ls\n", node->lineno,
                       (wchar_t *) node->symbol->ns);
      exit(1);
   }

   s.kind = SYM_MODULE;
   s.name = (mstring_t *) wcsdup ((wchar_t *) node->symbol->symbol);
   
   if (symtab_add_entry (stack, &s) == -1)
      duplicate_symbol_error (&s, node->lineno, stack);

   stack = enter_scope (stack);
   check_decl_lst (node->decl_lst, stack);
   stack = leave_scope (stack);
}

static void check_module_lst (absyn_module_lst_t *node, tabstack_t *stack)
{
   absyn_module_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_module_decl (tmp->module, stack);
      tmp = tmp->next;
   }
}

static void check_record_lst (absyn_record_lst_t *node, tabstack_t *stack)
{
   absyn_record_lst_t *tmp = node;

   while (tmp != NULL)
   {
      check_id (tmp->symbol, stack);
      check_expr (tmp->expr, stack);
      tmp = tmp->next;
   }
}

static void check_ty (absyn_ty_t *node, tabstack_t *stack)
{
   /* something about is_list */
   if (node->is_record)
      check_id_lst (node->record, stack);
   else
   {
      symbol_t s = { SYM_TYPE, (mstring_t *) node->identifier->symbol };

      if (!symtab_entry_exists (stack, &s))
         unknown_symbol_error (&s, node->lineno, stack);
   }
}

static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack)
{
   symbol_t *s = check_id (node->symbol, stack);

   s->kind = SYM_TYPE;
   if (symtab_add_entry (stack, s) == -1)
      duplicate_symbol_error (s, node->lineno, stack);

   check_ty (node->ty, stack);
}

static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack)
{
   symbol_t *s = check_id (node->symbol, stack);

   s->kind = SYM_FUNVAL;
   if (symtab_add_entry (stack, s) == -1)
      duplicate_symbol_error (s, node->lineno, stack);

   check_ty (node->ty, stack);
   check_expr (node->init, stack);
}

/* vim: set tags=../tags: */
