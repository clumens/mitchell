/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.18 2004/12/02 05:52:07 chris Exp $
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
#include <string.h>
#include <wchar.h>

#include "absyn.h"
#include "basic_types.h"
#include "error.h"
#include "memory.h"
#include "symtab.h"

/* This is the base environment, containing all the predefined values,
 * functions, modules, and types.  Keep this as absolutely small as possible.
 */
static ty_t boolean_ty = { TY_BOOLEAN };
static ty_t bottom_ty = { TY_BOTTOM };
static ty_t integer_ty = { TY_INTEGER };
static ty_t string_ty = { TY_STRING };

static symbol_t base_env[] = {
   { SYM_FUNVAL, &boolean_ty, L"f" },
   { SYM_FUNVAL, &boolean_ty, L"t" },
   { SYM_TYPE, &bottom_ty, L"⊥" },
   { SYM_TYPE, &boolean_ty, L"boolean" },
   { SYM_TYPE, &integer_ty, L"integer" },
   { SYM_TYPE, &string_ty, L"string" },
   { SYM_TYPE, NULL, NULL }
};

/* Maps a type to an identifying string.  Note that the order of these strings
 * must hatch the order of the type enumeration in basic_types.h.
 */
static char *ty_map[] = {
   "alias", "boolean", "bottom", "integer", "list", "record", "string"};

/* XXX: These are temporary environments to allow me to keep working on stuff
 * without getting stuck on the problem of how external modules get loaded in.
 * Of course, I'll need to figure that out before too long.
 */
static symbol_t integer_env[] = {
   { SYM_FUNVAL, NULL, L"+" },
   { SYM_FUNVAL, NULL, L"-" },
   { SYM_FUNVAL, NULL, L"*" },
   { SYM_FUNVAL, NULL, L"<" },
   { SYM_FUNVAL, NULL, L"=" },
   { SYM_FUNVAL, NULL, L"mod" },
   { SYM_TYPE, NULL, NULL }
};

static symbol_t boolean_env[] = {
   { SYM_FUNVAL, NULL, L"or" },
   { SYM_TYPE, NULL, NULL }
};

/* The global symbol table stack - always points to the outermost symbol table
 * (that is, the one all top-level modules get added into).
 */
static tabstack_t *global = NULL;

/* More mutually recursive functions for yet another tree walk. */
static ty_t *check_case_expr (absyn_case_expr_t *node, tabstack_t *stack);
static void check_decl (absyn_decl_t *node, tabstack_t *stack);
static ty_t *check_decl_expr (absyn_decl_expr_t *node, tabstack_t *stack);
static void check_decl_lst (absyn_decl_lst_t *node, tabstack_t *stack);
static ty_t *check_expr (absyn_expr_t *node, tabstack_t *stack);
static ty_t *check_expr_lst (absyn_expr_lst_t *node, tabstack_t *stack);
static void check_fun_decl (absyn_fun_decl_t *node, tabstack_t *stack);
static ty_t *check_id (absyn_id_expr_t *node, tabstack_t *stack);
static ty_t *check_if_expr (absyn_if_expr_t *node, tabstack_t *stack);
static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack);
static void check_module_lst (absyn_module_lst_t *node, tabstack_t *stack);
static void check_record_lst (absyn_record_lst_t *node, tabstack_t *stack);
static ty_t *check_ty (absyn_ty_t *node, tabstack_t *stack);
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
   MALLOC (integer_symtab, sizeof (symbol_t));
   integer_symtab->kind = SYM_MODULE;
   integer_symtab->name = wcsdup (L"Integer");
   integer_symtab->stack = enter_scope (integer_symtab->stack);

   symtab_add_entry (global, integer_symtab);

   for (i = 0; integer_env[i].name != NULL; i++)
      symtab_add_entry (integer_symtab->stack, &integer_env[i]);

   MALLOC (boolean_symtab, sizeof (symbol_t));
   boolean_symtab->kind = SYM_MODULE;
   boolean_symtab->name = wcsdup (L"Boolean");
   boolean_symtab->stack = enter_scope (boolean_symtab->stack);

   symtab_add_entry (global, boolean_symtab);

   for (i = 0; boolean_env[i].name != NULL; i++)
      symtab_add_entry (boolean_symtab->stack, &boolean_env[i]);

   check_module_lst (ast, global);
   global = leave_scope (global, L"global");
}

char *ty_to_str (const ty_t *ty)
{
   if (ty == NULL)
      return NULL;

   switch (ty->ty) {
      case TY_BOOLEAN:
      case TY_BOTTOM:
      case TY_INTEGER:
      case TY_STRING:
         return ty_map[ty->ty];
         break;

      case TY_ALIAS:
         return "some alias";
         break;

      case TY_LIST:
      {
         char *retval;
         char *tmp = ty_to_str(ty->list_base_ty);

         MALLOC(retval, 6);
         retval = strcpy (retval, "list ");
         REALLOC(retval, strlen(retval)+strlen(tmp)+1);
         retval = strcat (retval, tmp);

         return retval;
         break;
      }

      case TY_RECORD:
         return "some record";
         break;

      default:
         return NULL;
         break;
   }
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

/* A quick utility function to add SYM_FUNVALs into a symbol table, since we'll
 * be doing a lot of this.  It's sort of like what check_id used to do but
 * specialized to this case.
 */
static void add_simple_funval (absyn_id_expr_t *sym, tabstack_t *stack,
                               ty_t *ty)
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

   MALLOC (new, sizeof (symbol_t));
   new->name = wcsdup (sym->symbol);
   new->ty = ty;
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

   MALLOC (new, sizeof (symbol_t));
   new->name = wcsdup (sym->symbol);
   new->kind = SYM_TYPE;

   if (symtab_add_entry (stack, new) == -1)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, sym->lineno, sym->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }
}

/* Compare two types for equality. */
static unsigned int equal_types (ty_t *left, ty_t *right)
{
   /* Basic sanity checking. */
   if (left == NULL || right == NULL)
      return 0;

   /* If the two types are lists, apply the list-= rule, which states that two
    * list types are equivalent if the two underlying types are equivalent.
    * Of course if only one's a list, that's an error.
    */
   if (left->ty == TY_LIST && right->ty == TY_LIST)
   {
      return equal_types (left->list_base_ty, right->list_base_ty);
   }
   else if ((left->ty == TY_LIST && right->ty != TY_LIST) ||
            (left->ty != TY_LIST && right->ty == TY_LIST))
   {
      return 0;
   }
   else
   {
      return (left->ty == right->ty ? 1 : 0);
   }
}

/* +================================================================+
 * | TYPE CHECKING FUNCTIONS - ONE PER AST NODE TYPE                |
 * +================================================================+
 */

/* TODO:  need to check the branch tests to make sure they're a basic type
 * (see grammar)
 */
static ty_t *check_case_expr (absyn_case_expr_t *node, tabstack_t *stack)
{
   absyn_branch_lst_t *tmp = node->branch_lst;
   ty_t *test_ty = NULL;
   ty_t *branch_ty = NULL;
   ty_t *expr_ty = NULL;
   ty_t *t = NULL;

   /* Save the type of the test-expr for comparison against every branch. */
   test_ty = check_expr (node->test, stack);

   /* Check the branches.  We don't want a separate function for this for type
    * checking purposes.
    */
   while (tmp != NULL)
   {
      /* Compare each branch test to the first one, so if there is no first
       * one then we have to set it.  All branch tests must have the same
       * type.  This type must also be the same as the test-expr.  We'll only
       * compare the first branch's type to the test's type, since transitivity
       * will take care of the rest.
       */
      if (branch_ty == NULL)
      {
         branch_ty = check_expr (tmp->branch, stack);

         if (!equal_types (branch_ty, test_ty))
         {
            TYPE_ERROR (compiler_config.filename, tmp->branch->lineno,
                        "branch test must have the same type as case's test",
                        "branch test", ty_to_str(branch_ty), "test-expr",
                        ty_to_str(test_ty));
            exit(1);
         }
      }
      else
      {
         t = check_expr (tmp->branch, stack);

         if (!equal_types (branch_ty, t))
         {
            TYPE_ERROR (compiler_config.filename, tmp->branch->lineno,
                        "inconsistent types in case expression",
                        "previous test", ty_to_str (branch_ty), "this test",
                        ty_to_str(t));
            exit(1);
         }
      }

      /* Compare each branch expression to the first one, so if there is no
       * first one then we have to set it.  All branch expressions must have
       * the same type, and this is the type that the whole case expression
       * will return.
       */
      if (expr_ty == NULL)
         expr_ty = check_expr (tmp->expr, stack);
      else
      {
         t = check_expr (tmp->expr, stack);

         if (!equal_types (expr_ty, t))
         {
            TYPE_ERROR (compiler_config.filename, tmp->expr->lineno,
                        "inconsistent types in case branch exprs",
                        "previous expr", ty_to_str (expr_ty), "this expr",
                        ty_to_str(t));
            exit(1);
         }
      }

      tmp = tmp->next;
   }

   /* If there is a default expression, make sure it has the same type as
    * all the previous expressions.  It could also be the only branch, so take
    * care of that possibility as well.
    */
   if (node->default_expr != NULL)
   {
      if (expr_ty != NULL)
      {
         t = check_expr (node->default_expr, stack);

         if (!equal_types (expr_ty, t))
         {
            TYPE_ERROR (compiler_config.filename, node->default_expr->lineno,
                        "inconsistent types in case branch exprs",
                        "previous expr", ty_to_str (expr_ty), "default-expr",
                        ty_to_str(t));
            exit(1);
         }
      }
      else
         expr_ty = check_expr (node->default_expr, stack);
   }

   node->ty = expr_ty;
   return expr_ty;
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

static ty_t *check_decl_expr (absyn_decl_expr_t *node, tabstack_t *stack)
{
   stack = enter_scope (stack);
   check_decl_lst (node->decl_lst, stack);
   check_expr (node->expr, stack);
   stack = leave_scope (stack, L"decl-expr");

   node->ty = node->expr->ty;
   return node->ty;
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

static ty_t *check_expr (absyn_expr_t *node, tabstack_t *stack)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_BOOLEAN;
         break;

      case ABSYN_CASE:
         node->ty = check_case_expr (node->case_expr, stack);
         break;

      case ABSYN_DECL:
         node->ty = check_decl_expr (node->decl_expr, stack);
         break;

      case ABSYN_EXPR_LST:
         node->ty = check_expr_lst (node->expr_lst, stack);
         break;

      /* TODO: check types of arguments against types of formals, set type
       * to return type of function
       */
      case ABSYN_FUN_CALL:
         check_id (node->fun_call_expr.identifier, stack);
         /* check_expr_lst (node->fun_call_expr.arg_lst, stack); */
         break;

      case ABSYN_ID:
         node->ty = check_id (node->identifier, stack);
         break;

      case ABSYN_IF:
         node->ty = check_if_expr (node->if_expr, stack);
         break;

      case ABSYN_INTEGER:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_INTEGER;
         break;

      /* TODO: set type to the formed record */
      case ABSYN_RECORD_LST:
         check_record_lst (node->record_assn_lst, stack);
         break;

      case ABSYN_STRING:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_STRING;
         break;
   }

   return node->ty;
}

static ty_t *check_expr_lst (absyn_expr_lst_t *node, tabstack_t *stack)
{
   absyn_expr_lst_t *tmp = node;
   ty_t *t = NULL;
   ty_t *expr_ty = NULL;
   ty_t *retval = NULL;

   /* Check that each expression in the list has the same type as the first
    * expression in the list.
    */
   while (tmp != NULL)
   {
      if (expr_ty == NULL)
         expr_ty = check_expr (tmp->expr, stack);
      else
      {
         t = check_expr (tmp->expr, stack);

         if (!equal_types (expr_ty, t))
         {
            TYPE_ERROR (compiler_config.filename, tmp->expr->lineno,
                        "inconsistent types in expression list",
                        "previous expr", ty_to_str (expr_ty), "this expr",
                        ty_to_str(t));
            exit(1);
         }
      }

      tmp = tmp->next;
   }

   /* Now that we've verified all the expressions have the same type, create
    * a list type with the expression type as the base.
    */
   MALLOC(retval, sizeof(ty_t));
   retval->ty = TY_LIST;
   retval->list_base_ty = expr_ty;

   return retval;
}

/* TODO: add type information to symbol table entry */
static void check_fun_decl (absyn_fun_decl_t *node, tabstack_t *stack)
{
   absyn_id_lst_t *tmp;

   /* The function name needs to be entered into the outer scope since it is
    * able to be referenced from out there.
    */
   add_simple_funval (node->symbol, stack, NULL);
   check_ty (node->ty, stack);
   
   /* However, the parameters are only meaningful inside the function itself,
    * so enter a new level of scope before entering those.
    */
   stack = enter_scope (stack);

   /* TODO: add list of formal types into function's symtab entry. */
   for (tmp = node->id_lst; tmp != NULL; tmp = tmp->next)
   {
      add_simple_funval (tmp->symbol, stack, NULL);
      check_ty (tmp->ty, stack);
   }
   
   /* Now check the function body with this augmented environment. */
   check_expr (node->body, stack);
   stack = leave_scope (stack, node->symbol->symbol);
}

/* Check that an identifier exists somewhere in the symbol table. */
/* TODO: return type information */
static ty_t *check_id (absyn_id_expr_t *node, tabstack_t *stack)
{
   /* If there's no namespace, then this is just a naked identifier.  Traverse
    * the current module's symbol table stack from most local to the module's
    * top-level symbol table looking for an entry for the identifier.
    */
   if (node->sub == NULL)
   {
      if (!symtab_entry_exists (stack, node->symbol, SYM_FUNVAL))
      {
         BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                           node->symbol, "unknown symbol referenced");
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
            /* Look up the next part of the namespace path in the current
             * module's top-level symbol table (the part that's going to
             * contain entries for further modules).  If it's not found, that's
             * an error.
             */
            symbol_t *retval;

            if ((retval = table_lookup_entry (tbl, ns->symbol,
                                              SYM_MODULE)) ==NULL)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, ns->lineno,
                                 ns->symbol, "unknown symbol referenced");
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
            /* Okay, now we're down to just the naked identifier.  Look in
             * the current symbol table (no looking through a stack of tables)
             * to resolve the identifier.
             */
            if (!table_entry_exists (tbl, ns->symbol, SYM_FUNVAL))
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, ns->lineno,
                                 ns->symbol, "unknown symbol referenced");
               exit(1);
            }
            else
               break;
         }
      }
   }

   return NULL;
}

static ty_t *check_if_expr (absyn_if_expr_t *node, tabstack_t *stack)
{
   ty_t *tmp1, *tmp2;
   ty_t bool_ty = { TY_BOOLEAN };

   tmp1 = check_expr (node->test_expr, stack);
   if (!equal_types (tmp1, &bool_ty))
   {
      TYPE_ERROR (compiler_config.filename, node->lineno,
                  "if-expr test must return boolean type", "if-expr",
                  ty_to_str (tmp1), "expected", "boolean");
      exit(1);
   }
   
   tmp1 = check_expr (node->then_expr, stack);
   tmp2 = check_expr (node->else_expr, stack);

   if (!equal_types (tmp1, tmp2))
   {
      TYPE_ERROR (compiler_config.filename, node->else_expr->lineno,
                  "then-expr and else-expr must have the same type",
                  "then-expr", ty_to_str(tmp1), "else-expr", ty_to_str(tmp2));
      exit(1);
   }

   return tmp1;
}

static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack)
{
   symbol_t *new;

   if (node->symbol->sub != NULL)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                        node->symbol->symbol, "name may not contain a dot");
      exit(1);
   }

   MALLOC (new, sizeof (symbol_t));

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

static ty_t *check_ty (absyn_ty_t *node, tabstack_t *stack)
{
   ty_t *retval = NULL;

   switch (node->kind) {
      case ABSYN_TY_ID:
      {
         /* First check the local symbol table stack (to take into account any
          * modules we might be inside of).  If that fails, also check the
          * global symbol table for those basic types.
          */
         symbol_t *s = symtab_lookup_entry (stack, node->identifier->symbol,
                                            SYM_TYPE);

         if (s == NULL)
         {
            s = symtab_lookup_entry (global, node->identifier->symbol,
                                     SYM_TYPE);

            if (s == NULL)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                                 node->identifier->symbol,
                                 "unknown symbol referenced");
               exit(1);
            }
         }

         if (s->ty == NULL)
         {
            BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                              node->identifier->symbol, "symbol has no type");
            exit(1);
         }

         return s->ty;
      }

      /* Lists aren't so hard - make a type for the list and set its base
       * type to whatever's linked to by the AST node.
       */
      case ABSYN_TY_LIST:
         MALLOC(retval, sizeof(ty_t));
         retval->ty = TY_LIST;
         retval->list_base_ty = check_ty (node->list, stack);
         return retval;

      /* Records are a little bit complicated. */
      case ABSYN_TY_RECORD:
      {
         absyn_id_lst_t *cur = node->record;
         absyn_id_lst_t *tmp;

         /* Step 1.  Check all record identifiers to make sure they're not
          * trying to be in some sort of namespace.
          */
         for (tmp = cur; tmp != NULL; tmp = tmp->next)
         {
            if (tmp->symbol->sub != NULL)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, tmp->lineno,
                                 tmp->symbol->symbol,
                                 "symbol may not contain a namespace");
               exit(1);
            }
         }

         /* Step 2.  Check all record identifiers for proper typing and no
          * duplicates.
          */
         while (cur != NULL)
         {
            /* First, check the type of the record member. */
            check_ty (cur->ty, stack);

            /* Now make sure there's no other member with the same name. */
            for (tmp = cur->next; tmp != NULL; tmp = tmp->next)
            {
               if (wcscmp (tmp->symbol->symbol, cur->symbol->symbol) == 0)
               {
                  BAD_SYMBOL_ERROR (compiler_config.filename, tmp->lineno,
                                    tmp->symbol->symbol, "duplicate symbol "
                                    "already exists in this scope");
                  exit(1);
               }
            }

            cur = cur->next;
         }

         return NULL;
      }
   }

   return NULL;
}

/* TODO:  handle new types */
static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack)
{
   add_simple_type (node->symbol, stack);
   check_ty (node->ty, stack);
}

static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack)
{
   ty_t *val_ty, *expr_ty;

   val_ty = check_ty (node->ty, stack);
   expr_ty = check_expr (node->init, stack);

   if (!equal_types (val_ty, expr_ty))
   {
      TYPE_ERROR (compiler_config.filename, node->lineno,
                  "type of value initializer does not match value's type",
                  "declared", ty_to_str (val_ty), "initializer",
                  ty_to_str(expr_ty));
      exit(1);
   }

   add_simple_funval (node->symbol, stack, val_ty);
}

/* vim: set tags=../tags: */
