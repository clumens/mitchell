/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.10 2004/11/24 20:45:40 chris Exp $
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
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

#include "absyn.h"
#include "error.h"
#include "memory.h"
#include "symtab.h"

/* This is the base environment, containing all the predefined values,
 * functions, modules, and types.  Keep this as absolutely small as possible.
 */
static symbol_t base_env[] = {
   { SYM_FUNVAL, L"f" },
   { SYM_FUNVAL, L"t" },
   { SYM_TYPE, L"⊥" },
   { SYM_TYPE, L"boolean" },
   { SYM_TYPE, L"integer" },
   { SYM_TYPE, L"string" },
   { SYM_TYPE, NULL }
};

/* XXX: These are temporary environments to allow me to keep working on stuff
 * without getting stuck on the problem of how external modules get loaded in.
 * Of course, I'll need to figure that out before too long.
 */
static symbol_t integer_env[] = {
   { SYM_FUNVAL, L"+" },
   { SYM_FUNVAL, L"-" },
   { SYM_FUNVAL, L"*" },
   { SYM_FUNVAL, L"<" },
   { SYM_FUNVAL, L"=" },
   { SYM_FUNVAL, L"mod" },
   { SYM_TYPE, NULL }
};

static symbol_t boolean_env[] = {
   { SYM_FUNVAL, L"or" },
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
   symbol_t *integer_symtab, *boolean_symtab;
   unsigned int i;

   global = enter_scope (global);

   /* Add the base environment to the global symbol table. */
   for (i = 0; base_env[i].name != NULL; i++)
      symtab_add_entry (global, &base_env[i]);

   /* XXX: This is temporary stuff.  Create module symtabs for Integer and
    * Boolean, and populate those tables.  This will allow us to continue
    * running the test cases and not getting stuck.
    */
   MALLOC (integer_symtab, sizeof (symbol_t))
   integer_symtab->kind = SYM_MODULE;
   integer_symtab->name = wcsdup (L"Integer");
   integer_symtab->stack = enter_scope (integer_symtab->stack);

   symtab_add_entry (global, integer_symtab);

   for (i = 0; integer_env[i].name != NULL; i++)
      symtab_add_entry (integer_symtab->stack, &integer_env[i]);

   MALLOC (boolean_symtab, sizeof (symbol_t))
   boolean_symtab->kind = SYM_MODULE;
   boolean_symtab->name = wcsdup (L"Boolean");
   boolean_symtab->stack = enter_scope (boolean_symtab->stack);

   symtab_add_entry (global, boolean_symtab);

   for (i = 0; boolean_env[i].name != NULL; i++)
      symtab_add_entry (boolean_symtab->stack, &boolean_env[i]);

   check_module_lst (ast, global);
   global = leave_scope (global, L"global");
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
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, sym->lineno, sym->symbol,
                        "unknown symbol referenced");
      exit(1);
   }

   MALLOC (new, sizeof (symbol_t))
   new->name = wcsdup (sym->symbol);
   new->kind = SYM_FUNVAL;

   if (symtab_add_entry (stack, new) == -1)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, sym->lineno, sym->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }
}

/* Similar to the above, except for types. */
static void add_simple_type (absyn_id_expr_t *sym, tabstack_t *stack)
{
   symbol_t *new = NULL;

   /* Make sure these TYPEs do not contain periods, as that wouldn't make
    * them simple anymore.
    */
   if (sym->sub != NULL)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, sym->lineno, sym->symbol,
                        "symbol may not contain a namespace");
      exit(1);
   }

   MALLOC (new, sizeof (symbol_t))
   new->name = wcsdup (sym->symbol);
   new->kind = SYM_TYPE;

   if (symtab_add_entry (stack, new) == -1)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, sym->lineno, sym->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }
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
   stack = leave_scope (stack, L"decl-expr");
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

      /* TODO: check types of arguments against types of formals */
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

   /* TODO: add list of formal types into function's symtab entry. */
   for (tmp = node->id_lst; tmp != NULL; tmp = tmp->next)
   {
      add_simple_funval (tmp->symbol, stack);
      check_ty (tmp->ty, stack);
   }
   
   /* Now check the function body with this augmented environment. */
   check_expr (node->body, stack);
   stack = leave_scope (stack, node->symbol->symbol);
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
      symbol_t s = { SYM_FUNVAL, node->symbol };

      if (!symtab_entry_exists (stack, &s))
      {
         BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno, s.name,
                           "unknown symbol referenced");
         exit(1);
      }
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
         if (ns->sub != NULL)
         {
            symbol_t s = { SYM_MODULE, ns->symbol };

            /* Look up the next part of the namespace path in the current
             * module's top-level symbol table (the part that's going to
             * contain entries for further modules).  If it's not found, that's
             * an error.
             */
            symbol_t *retval;

            if ((retval = lookup_entry (tbl, s.name, s.kind)) == NULL)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, ns->lineno, s.name,
                                 "unknown symbol referenced");
               exit(1);
            }

            /* Really, this should never happen (kiss of death, I know). */
            assert (retval != NULL);
            assert (retval->stack != NULL);
            assert (retval->stack->symtab != NULL);

            /* Traverse down into the next module's symbol table and strip off
             * one layer of the namespace path from the identifier to set up
             * for another pass.
             */
            tbl = retval->stack->symtab;
            ns = ns->sub;
         }
         else
         {
            symbol_t s = { SYM_FUNVAL, ns->symbol };

            /* Okay, now we're down to just the naked identifier.  Look in
             * the current symbol table (no looking through a stack of tables)
             * to resolve the identifier.
             */
            if (!table_entry_exists (tbl, &s))
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, ns->lineno, s.name,
                                 "unknown symbol referenced");
               exit(1);
            }
            else
               break;
         }
      }
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
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                        node->symbol->symbol,
                        "symbol may not contain a namespace");
      exit(1);
   }

   MALLOC (new, sizeof (symbol_t))

   /* Build symtab entry in lexical parent's table for this module. */
   new->kind = SYM_MODULE;
   new->name = wcsdup (node->symbol->symbol);
   new->stack = enter_scope (new->stack);
   
   /* Add the module's symbol table entry, with its pointer to initialized
    * inner symbol table.
    */
   if (symtab_add_entry (stack, new) == -1)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                        node->symbol->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }

   /* Check the guts of the module against the module's new environment. */
   check_decl_lst (node->decl_lst, new->stack);

   if (compiler_config.debug.dump_symtabs)
      symtab_dump (new->stack, node->symbol->symbol);
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

/* FIXME:  This is all wrong. */
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

/* TODO:  something about is_list */
static void check_ty (absyn_ty_t *node, tabstack_t *stack)
{
   /* Records are a little bit complicated. */
   if (node->is_record)
   {
      absyn_id_lst_t *cur = node->record;
      absyn_id_lst_t *tmp;

      /* Step 1.  Check all record identifiers to make sure they're not trying
       * to be in some sort of namespace.
       */
      for (tmp = cur; tmp != NULL; tmp = tmp->next)
         if (tmp->symbol->sub != NULL)
         {
            BAD_SYMBOL_ERROR (compiler_config.filename, tmp->lineno,
                              tmp->symbol->symbol,
                              "symbol may not contain a namespace");
            exit(1);
         }

      /* Step 2.  Check all record identifiers for proper typing and no
       * duplicates.
       */
      while (cur != NULL)
      {
         /* First, check the type of the record member. */
         check_ty (cur->ty, stack);

         /* Now make sure there's no other record member with the same name. */
         for (tmp = cur->next; tmp != NULL; tmp = tmp->next)
            if (wcscmp (tmp->symbol->symbol, cur->symbol->symbol) == 0)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, tmp->lineno,
                                 tmp->symbol->symbol, "duplicate symbol "
                                 "already exists in this scope");
               exit(1);
            }

         cur = cur->next;
      }
   }
   else
   {
      symbol_t s = { SYM_TYPE, node->identifier->symbol };

      /* First check the local symbol table stack (to take into account any
       * modules we might be inside of).  If that fails, also check the global
       * symbol table for those basic types.
       */
      if (!symtab_entry_exists (stack, &s) && !symtab_entry_exists (global, &s))
      {
         BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno, s.name,
                           "unknown symbol referenced");
         exit(1);
      }
   }
}

static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack)
{
   add_simple_type (node->symbol, stack);
   check_ty (node->ty, stack);
}

static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack)
{
   add_simple_funval (node->symbol, stack);
   check_ty (node->ty, stack);
   check_expr (node->init, stack);
}

/* vim: set tags=../tags: */
