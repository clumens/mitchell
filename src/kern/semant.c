/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.4 2004/11/21 05:42:04 chris Exp $
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

/* The global symbol table stack - always points to the outermost symbol table
 * (that is, the one all top-level modules get added into).
 */
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
static void check_id (absyn_id_expr_t *node, tabstack_t *stack);
static void check_id_lst (absyn_id_lst_t *node, tabstack_t *stack);
static void check_if_expr (absyn_if_expr_t *node, tabstack_t *stack);
static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack);
static void check_module_lst (absyn_module_lst_t *node, tabstack_t *stack);
static void check_record_lst (absyn_record_lst_t *node, tabstack_t *stack);
static void check_ty (absyn_ty_t *node, tabstack_t *stack);
static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack);
static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack);

/* Semantic analysis entry point. */
void check_program (ast_t *ast)
{
   unsigned int i;

   global = enter_scope (global);

   for (i = 0; base_env[i].name != NULL; i++)
      symtab_add_entry (global, &base_env[i]);

   check_module_lst (ast, global);
   global = leave_scope (global);
}

/* +================================================================+
 * | COMMON ERROR HANDLING FUNCTIONS                                |
 * +================================================================+
 */

static void duplicate_symbol_error (symbol_t *s, unsigned int lineno,
                                    tabstack_t *stack)
{
   fprintf (stderr, "*** duplicate symbol referenced on line %d:  %ls\n",
                    lineno, (wchar_t *) s->name);
   if (stack != NULL) symtab_dump (stack);
   exit (1);
}

static void invalid_name_error (absyn_id_expr_t *ns, unsigned int lineno,
                                tabstack_t *stack)
{
   fprintf (stderr, "*** error on line %d:  name includes "
                    "namespace:  %ls\n", lineno, (wchar_t *) ns->symbol);
   if (stack != NULL) symtab_dump (stack);
   exit (1);
}

static void unknown_symbol_error (symbol_t *s, unsigned int lineno,
                                  tabstack_t *stack)
{
   fprintf (stderr, "*** unknown symbol referenced on line %d:  %ls\n",
                    lineno, (wchar_t *) s->name);
   if (stack != NULL) symtab_dump (stack);
   exit (1);
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

/* A quick utility function to add SYM_FUNVALs into a symbol table, since we'll
 * be doing a lot of this.  It's sort of like what check_id used to do but
 * specialized to this case.
 */
static void add_simple_funval (absyn_id_expr_t *sym, tabstack_t *stack)
{
   symbol_t *new = NULL;

   /* Make sure these FUNVALs do not contain periods, as that wouldn't make
    * them simple anymore.
    */
   if (sym->sub != NULL)
      invalid_name_error (sym, sym->lineno, stack);

   MALLOC (new, sizeof (symbol_t))
   new->name = (mstring_t *) wcsdup ((wchar_t *) sym->symbol);
   new->kind = SYM_FUNVAL;

   if (symtab_add_entry (stack, new) == -1)
      duplicate_symbol_error (new, sym->lineno, stack);
}

/* +================================================================+
 * | TYPE CHECKING FUNCTIONS - ONE PER AST NODE TYPE                |
 * +================================================================+
 */

/* TODO: Check for duplicate branches. */
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
         check_id (node->identifier, stack);
         break;

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
   absyn_id_lst_t *tmp;

   /* The function name needs to be entered into the outer scope since it is
    * able to be referenced from out there.
    */
   add_simple_funval (node->symbol, stack);
   check_ty (node->ty, stack);
   
   /* However, the parameters are only meaningful inside the function itself,
    * so enter a new level of scope before entering those.
    */
   stack = enter_scope (stack);

   for (tmp = node->id_lst; tmp != NULL; tmp = tmp->next)
   {
      add_simple_funval (tmp->symbol, stack);
      check_ty (tmp->ty, stack);
   }
   
   /* Now check the function body with this augmented environment. */
   check_expr (node->body, stack);
   stack = leave_scope (stack);
}

/* Check that an identifier exists somewhere in the symbol table. */
static void check_id (absyn_id_expr_t *node, tabstack_t *stack)
{
   /* If there's no namespace, then this is just a naked identifier.  Traverse
    * the current module's symbol table stack from most local to the module's
    * top-level symbol table looking for an entry for the identifier.
    */
   if (node->sub == NULL)
   {
      symbol_t s = { SYM_FUNVAL, (mstring_t *) node->symbol };

      if (!symtab_entry_exists (stack, &s))
         unknown_symbol_error (&s, node->lineno, stack);
   }
   else
   {
      /* Since a namespace is specified, it must be from the top-level all
       * the way down.  So, intialize our table pointer to the global symbol
       * table.
       */
      absyn_id_expr_t *ns = node;
      symtab_t *tbl = global->symtab;

      while (1)
      {
         symbol_t s = { SYM_FUNVAL, (mstring_t *) ns->symbol };

         if (ns->sub != NULL)
         {
            /* Look up the next part of the namespace path in the current
             * module's top-level symbol table (the part that's going to
             * contain entries for further modules).  If it's not found, that's
             * an error.
             */
            symbol_t *retval = lookup_entry (tbl, s.name, s.kind);
            if (retval == NULL)
               unknown_symbol_error (&s, ns->lineno, stack);

            /* Traverse down into the next module's symbol table and strip off
             * one layer of the namespace path from the identifier to set up
             * for another pass.
             */
            tbl = retval->stack->symtab;
            ns = ns->sub;
         }
         else
         {
            /* Okay, now we're down to just the naked identifier.  Look in
             * the current symbol table (no looking through a stack of tables)
             * to resolve the identifier.
             */
            if (!table_entry_exists (tbl, &s))
               unknown_symbol_error (&s, ns->lineno, NULL);
            else
               break;
         }
      }
   }
}

/* TODO: Check for duplicate names. */
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
   symbol_t *new;

   if (node->symbol->sub != NULL)
      invalid_name_error (node->symbol->sub, node->lineno, stack);

   MALLOC (new, sizeof (symbol_t))

   /* Build symtab entry in lexical parent's table for this module. */
   new->kind = SYM_MODULE;
   new->name = (mstring_t *) wcsdup ((wchar_t *) node->symbol->symbol);
   new->stack = enter_scope (new->stack);
   
   /* Add the module's symbol table entry, with its pointer to initialized
    * inner symbol table.
    */
   if (symtab_add_entry (stack, new) == -1)
      duplicate_symbol_error (new, node->lineno, stack);

   /* Check the guts of the module against the module's new environment. */
   check_decl_lst (node->decl_lst, new->stack);
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

      /* First check the local symbol table stack (to take into account any
       * modules we might be inside of).  If that fails, also check the global
       * symbol table for those basic types.
       */
      if (!symtab_entry_exists (stack, &s) && !symtab_entry_exists (global, &s))
         unknown_symbol_error (&s, node->lineno, stack);
   }
}

static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack)
{
#if 0
   symbol_t *s = check_id (node->symbol, stack);

   s->kind = SYM_TYPE;
   if (symtab_add_entry (stack, s) == -1)
      duplicate_symbol_error (s, node->lineno, stack);
#endif
   check_id (node->symbol, stack);
   check_ty (node->ty, stack);
}

static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack)
{
   add_simple_funval (node->symbol, stack);
   check_ty (node->ty, stack);
   check_expr (node->init, stack);
}

/* vim: set tags=../tags: */
