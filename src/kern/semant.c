/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.40 2005/04/06 02:22:15 chris Exp $
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
#include "error.h"
#include "list.h"
#include "memory.h"
#include "symtab.h"

/* This is the base environment, containing all the predefined values,
 * functions, modules, and types.  Keep this as absolutely small as possible.
 */
static ty_t boolean_ty = { TY_BOOLEAN, TY_FINITE };
static ty_t bottom_ty = { TY_BOTTOM, TY_FINITE };
static ty_t integer_ty = { TY_INTEGER, TY_FINITE };
static ty_t string_ty = { TY_STRING, TY_FINITE };

static symbol_t base_env[] = {
   { SYM_VALUE, L"f", .info.ty=&boolean_ty },
   { SYM_VALUE, L"t", .info.ty=&boolean_ty },
   { SYM_TYPE, L"⊥", .info.ty=&bottom_ty },
   { SYM_TYPE, L"boolean", .info.ty=&boolean_ty },
   { SYM_TYPE, L"integer", .info.ty=&integer_ty },
   { SYM_TYPE, L"string", .info.ty=&string_ty },
   { SYM_TYPE, NULL, .info.ty=NULL }
};

/* XXX: These are temporary environments to allow me to keep working on stuff
 * without getting stuck on the problem of how external modules get loaded in.
 * Of course, I'll need to figure that out before too long.
 */
static symbol_t integer_env[] = {
   { SYM_FUNCTION, L"+", .info.ty=NULL },
   { SYM_FUNCTION, L"-", .info.ty=NULL },
   { SYM_FUNCTION, L"*", .info.ty=NULL },
   { SYM_FUNCTION, L"<", .info.ty=NULL },
   { SYM_FUNCTION, L"=", .info.ty=NULL },
   { SYM_FUNCTION, L"mod", .info.ty=NULL },
   { SYM_TYPE, NULL, .info.ty=NULL }
};

static symbol_t boolean_env[] = {
   { SYM_FUNCTION, L"or", .info.ty=NULL },
   { SYM_TYPE, NULL, .info.ty=NULL }
};
/* XXX: End temporary stuff. */

/* The global symbol table stack - always points to the outermost symbol table
 * (that is, the one all top-level modules get added into).
 */
static tabstack_t *global = NULL;

/* More mutually recursive functions for yet another tree walk. */
static ty_t *check_case_expr (absyn_case_expr_t *node, tabstack_t *stack);
static ty_t *check_decl_expr (absyn_decl_expr_t *node, tabstack_t *stack);
static void check_decl_lst (list_t *lst, tabstack_t *stack);
static ty_t *check_exn_expr (absyn_exn_expr_t *node, tabstack_t *stack);
static ty_t *check_exn_handler (absyn_exn_handler_t *node, tabstack_t *stack);
static ty_t *check_exn_lst (absyn_exn_lst_t *node, tabstack_t *stack);
static ty_t *check_expr (absyn_expr_t *node, tabstack_t *stack);
static ty_t *check_expr_lst (list_t *lst, tabstack_t *stack);
static ty_t *check_fun_call (absyn_fun_call_t *node, tabstack_t *stack);
static void check_fun_decl (absyn_fun_decl_t *node, tabstack_t *stack);
static ty_t *check_id (absyn_id_expr_t *node, tabstack_t *stack);
static ty_t *check_if_expr (absyn_if_expr_t *node, tabstack_t *stack);
static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack);
static void check_module_lst (list_t *lst, tabstack_t *stack);
static ty_t *check_record_assn (list_t *lst, tabstack_t *stack);
static ty_t *check_record_ref (absyn_record_ref_t *node, tabstack_t *stack);
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
   integer_symtab->info.stack = enter_scope (integer_symtab->info.stack);

   symtab_add_entry (global, integer_symtab);

   for (i = 0; integer_env[i].name != NULL; i++)
      symtab_add_entry (integer_symtab->info.stack, &integer_env[i]);

   MALLOC (boolean_symtab, sizeof (symbol_t));
   boolean_symtab->kind = SYM_MODULE;
   boolean_symtab->name = wcsdup (L"Boolean");
   boolean_symtab->info.stack = enter_scope (boolean_symtab->info.stack);

   symtab_add_entry (global, boolean_symtab);

   for (i = 0; boolean_env[i].name != NULL; i++)
      symtab_add_entry (boolean_symtab->info.stack, &boolean_env[i]);
   /* XXX: End temporary stuff. */

   check_module_lst (ast, global);

   global = leave_scope (global, L"global");
}

#define WCSLEN(str)  (wcslen(str)*sizeof(wchar_t))

/* Convert a ty_t into a string representation for debugging output purposes. */
wchar_t *ty_to_str (const ty_t *ty)
{
   /* Maps a type to an identifying string.  Note that the order of these
    * strings must match the order of the ty_kind enumeration in symtab.h.
    */
   static wchar_t *ty_map[] = {
      L"alias", L"any", L"boolean", L"⊥", L"exn", L"integer", L"list",
      L"record", L"string" };

   if (ty == NULL)
      return NULL;

   switch (ty->ty) {
      case TY_ANY:
      case TY_BOOLEAN:
      case TY_BOTTOM:
      case TY_INTEGER:
      case TY_STRING:
         return ty_map[ty->ty];
         break;

      case TY_ALIAS:
         return ty->alias->name;
         break;

      case TY_LIST:
      {
         wchar_t *retval;
         wchar_t *tmp = ty_to_str(ty->list_base_ty);

         MALLOC(retval, sizeof(wchar_t)*6);
         retval = wcscpy (retval, L"list ");
         REALLOC(retval, WCSLEN(retval)+WCSLEN(tmp)+1);
         retval = wcscat (retval, tmp);

         return retval;
         break;
      }

      case TY_EXN:
      case TY_RECORD:
      {
         wchar_t *retval, *tmp;
         list_t *lst;
         element_t *ele;

         /* First, the brace indicating a record type. */
         if (ty->ty == TY_EXN)
         {
            MALLOC(retval, sizeof(wchar_t)*5);
            retval = wcscpy (retval, L"EXN{");
         }
         else
         {
            MALLOC(retval, sizeof(wchar_t)*2);
            retval = wcscpy (retval, L"{");
         }

         for (lst = ty->elts; lst != NULL; lst = lst->next)
         {
            ele = (element_t *) lst->data;

            /* Second, the name of the element. */
            REALLOC(retval, WCSLEN(retval)+WCSLEN(ele->identifier));
            retval = wcscat (retval, ele->identifier);

            /* Third, a colon. */
            REALLOC(retval, WCSLEN(retval)+sizeof(wchar_t)*2);
            retval = wcscat (retval, L":");

            /* Fourth, the type of the element. */
            tmp = ty_to_str(ele->ty);
            REALLOC(retval, WCSLEN(retval)+WCSLEN(tmp)+1);
            retval = wcscat (retval, tmp);

            /* Fifth, a comma. */
            if (lst->next != NULL)
            {
               REALLOC(retval, WCSLEN(retval)+sizeof(wchar_t)*2);
               retval = wcscat (retval, L",");
            }
         }

         /* Finally, the closing brace. */
         REALLOC(retval, WCSLEN(retval)+sizeof(wchar_t)*2);
         retval = wcscat (retval, L"}");

         return retval;
         break;
      }

#ifndef NEW_TYPES
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad ty->ty");
         exit(1);
#endif
   }

   return NULL;
}

/* Strip off any aliases present on a type. */
ty_t *unalias (ty_t *ty)
{
   ty_t *retval = ty;

   while (retval->ty == TY_ALIAS)
      retval = retval->alias->info.ty;

   return retval;
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

static int __ele_to_ele_cmp (void *lst_data, void *user_data)
{
   return wcscmp (((element_t *) lst_data)->identifier,
                  ((element_t *) user_data)->identifier);
}

static int __ele_to_str_cmp (void *lst_data, void *user_data)
{
   return wcscmp (((element_t *) lst_data)->identifier, (wchar_t *) user_data);
}

/* Like equal_types, but check to see if ty is a specific type instead of
 * straight equality testing.
 */
static unsigned int is_ty_kind (ty_t *ty, ty_kind kind)
{
   if (ty == NULL)
      return 0;

   if (ty->ty == TY_ALIAS)
      return (unalias (ty))->ty == kind;
   else
      return ty->ty == kind;
}

/* Compare two types for equality, after first stripping off any aliases
 * they may have.  We can do this because our typing rules state that two
 * aliased types are equal if they bottom out in the same types.
 */
static unsigned int equal_types (ty_t *aliased_left, ty_t *aliased_right)
{
   ty_t *left, *right;

   /* Basic sanity checking. */
   if (aliased_left == NULL || aliased_right == NULL)
      return 0;

   /* Strip off the aliases. */
   left = unalias (aliased_left);
   right = unalias (aliased_right);

   /* If either type is TY_ANY, they're equal. */
   if (left->ty == TY_ANY || right->ty == TY_ANY)
      return 1;

   switch (left->ty) {
      /* Two list types are equal if their base types are equal. */
      case TY_LIST:
         if (right->ty == TY_LIST)
            return equal_types (left->list_base_ty, right->list_base_ty);
         else
            return 0;

      /* Two record types are equal if they have identical lists of elements,
       * each with identical types.  No fancy subtyping here.  The same rules
       * apply for exceptions, which are basically just dressed up records.
       */
      case TY_EXN:
      case TY_RECORD:
      {
         list_t *left_lst, *right_lst;
         element_t *left_ele, *right_ele;

         /* Records and exceptions are not equal, though. */
         if (left->ty == right->ty)
         {
            left_lst = left->elts;
            right_lst = right->elts;
         }
         else
            return 0;

         while (1)
         {
            /* If both lists are at NULL, they were the same length and passed
             * all the other tests, so return success.  If only one list is
             * at NULL, they weren't the same length so fail.
             */
            if (left_lst == NULL && right_lst == NULL)
               break;
            else if (!(left_lst != NULL && right_lst != NULL))
               return 0;

            left_ele = (element_t *) left_lst->data;
            right_ele = (element_t *) right_lst->data;

            /* If the elements do not have the same identifier with the same
             * type, fail.  Otherwise, we cycle around to the next one.
             */
            if (wcscmp (left_ele->identifier, right_ele->identifier) != 0 ||
                !equal_types (left_ele->ty, right_ele->ty))
               return 0;

            left_lst = left_lst->next;
            right_lst = right_lst->next;
         }

         return 1;
      }

      /* Otherwise, two types are equal if they're the same type. */
      default:
         return left->ty == right->ty;
   }

   return 0;
}

/* Look up the identifier provided by node in the given symbol table stack.
 * Returns the symbol table entry if it exists (from which type information
 * can be extracted) or NULL otherwise.
 */
static symbol_t *lookup_id (absyn_id_expr_t *node, subtable_t kind,
                            tabstack_t *stack)
{
   /* If there's no namespace, this is just a naked identifier.  That means
    * it must be resolved within the current module.  Traverse the current
    * module's symbol table stack from most local to the top-level symbol
    * table looking for a matching entry.
    */
   if (node->sub == NULL)
      return symtab_lookup_entry (stack, node->symbol, kind);
   else
   {
      /* Since a namespace is specified, it must specify the entire module
       * path from the globally available one all the way down.  The general
       * algorithm is to look at the first namespace in the ID, check the
       * global symbol table for that module, enter into that module and strip
       * off the outermost namespace from the ID, and continue in this fashion.
       */
      absyn_id_expr_t *ns  = node;
      symtab_t        *tbl = global->symtab;

      while (1)
      {
         if (ns->sub != NULL)
         {
            /* Look up the next part of the namespace path in the current
             * module's top-level symbol table (the part that's going to
             * contain entries for further modules).  If it's not found, that's
             * an error.
             */
            symbol_t *entry = table_lookup_entry (tbl, ns->symbol, SYM_MODULE);

            if (entry == NULL)
               return NULL;

            /* Traverse down into the next module's symbol table and strip off
             * one layer of the namespace path from the ID to set up for
             * another pass.
             */
            tbl = entry->info.stack->symtab;
            ns = ns->sub;
         }
         else
            /* Okay, now we're down to just the naked ID.  Look in the current
             * symbol table (no looking through a stack) to resolve.
             */
            return table_lookup_entry (tbl, ns->symbol, kind);
      }
   }

   return NULL;
}

/* First look up the symbol in the provided stack.  If it's not found there,
 * check the global symbol table.
 */
static symbol_t *lookup_id_global (absyn_id_expr_t *node, subtable_t kind,
                                   tabstack_t *stack)
{
   symbol_t *s;

   if ((s = lookup_id (node, kind, stack)) == NULL)
      return lookup_id (node, kind, global);
   else
      return s;
}

/* Convert an AST representation of a type into a corresponding symbol table
 * type declaration, suitable for inserting into tables.
 */
static ty_t *ast_to_ty (absyn_ty_t *node, tabstack_t *stack)
{
   ty_t *retval = NULL;

   switch (node->kind) {
      case ABSYN_TY_BOTTOM:
         MALLOC(retval, sizeof(ty_t));
         retval->ty = TY_BOTTOM;
         break;

      case ABSYN_TY_ID:
      {
         /* First check the local symbol table stack (to take into account any
          * modules we might be inside of).  If that fails, also check the
          * global symbol table for those basic types.
          */
         symbol_t *s;

         if ((s = lookup_id_global (node->identifier, SYM_TYPE, stack)) == NULL)
         {
            BAD_SYMBOL_ERROR (cconfig.filename, node->lineno,
                              node->column, node->identifier->symbol,
                              "unknown symbol referenced");
            exit(1);
         }
         else if (s->info.ty != NULL)
         {
            MALLOC (retval, sizeof(ty_t));
            retval->ty = TY_ALIAS;
            retval->alias = s;
            break;
         }

         /* If the type pointer is NULL, that means this is an alias to a
          * type that's defined later on in the decl-lst.  Since the type
          * pointer in the target's symbol table entry will be filled in later,
          * for now all we can return is a made-up type that points to that
          * symbol table entry.
          */
         if (s->info.ty == NULL)
         {
            MALLOC(retval, sizeof(ty_t));
            retval->ty = TY_ALIAS;
            retval->alias = s;
         }
         else
            retval = s->info.ty;

         break;
      }

      case ABSYN_TY_LIST:
         MALLOC(retval, sizeof(ty_t));
         retval->ty = TY_LIST;
         retval->list_base_ty = ast_to_ty (node->list, stack);
         break;

      /* These two are so similar that they can be one case, with a little
       * caution taken.
       */
      case ABSYN_TY_EXN:
      case ABSYN_TY_RECORD:
      {
         element_t *new_ele;
         list_t *tmp;

         MALLOC (retval, sizeof(ty_t));
         retval->elts = NULL;

         if (node->kind == ABSYN_TY_EXN)
         {
            retval->ty = TY_EXN;
            tmp = node->exn;
         }
         else
         {
            retval->ty = TY_RECORD;
            tmp = node->record;
         }

         /* Store all the record elements in alphabetical order in the symbol,
          * since that will make various record operations easier later on.
          */
         for ( /* initialized above */ ; tmp != NULL; tmp = tmp->next)
         {
            absyn_id_lst_t *cur_id = tmp->data;

            MALLOC (new_ele, sizeof(element_t));
            new_ele->identifier = cur_id->symbol->symbol;
            new_ele->ty = ast_to_ty (cur_id->ty, stack);

            /* Two elements with the same name are not allowed. */
            retval->elts = list_insert_unique (retval->elts, new_ele,
                                               __ele_to_ele_cmp);
            if (retval->elts == NULL)
            {
               BAD_SYMBOL_ERROR (cconfig.filename, cur_id->lineno,
                                 cur_id->column, cur_id->symbol->symbol,
                                 "duplicate symbol already exists in this "
                                 "record type");
               exit(1);
            }
         }

         break;
      }

#ifndef NEW_GRAMMAR
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad node->kind");
         exit(1);
#endif
   }

   retval->is_finite = TY_UNVISITED;
   return retval;
}

/* Check that a type does not form an infinite loop.  All the basic types are
 * defined to not form a loop, providing us with a base case.  Then, all other
 * types must be shown to ground out into something that also does not form
 * a loop.
 */
static ty_finite ty_is_finite (ty_t *ty)
{
   ty->is_finite = TY_VISITED;

   switch (ty->ty) {
      case TY_ANY:
      case TY_BOOLEAN:
      case TY_BOTTOM:
      case TY_INTEGER:
      case TY_STRING:
         ty->is_finite = TY_FINITE;
         break;

      case TY_ALIAS:
         if (ty->alias->info.ty->is_finite == TY_VISITED)
            ty->is_finite = TY_NOT_FINITE;
         else
            ty->is_finite = ty_is_finite (ty->alias->info.ty);
         break;

      case TY_LIST:
         if (ty->list_base_ty->is_finite == TY_VISITED)
            ty->is_finite = TY_NOT_FINITE;
         else
            ty->is_finite = ty_is_finite (ty->list_base_ty);
         break;

      /* Just assume these are okay for now. */
      case TY_EXN:
      case TY_RECORD:
         ty->is_finite = TY_FINITE;
         break;

#ifndef NEW_TYPES
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad ty->ty");
         exit(1);
#endif
   }

   return ty->is_finite;
}

/* +================================================================+
 * | TYPE CHECKING FUNCTIONS - ONE PER AST NODE TYPE                |
 * +================================================================+
 */

static ty_t *check_case_expr (absyn_case_expr_t *node, tabstack_t *stack)
{
   list_t *tmp;

   /* Check the type of the test expression, which must currently be one of
    * the basic types.  All branch tests will also have to have the same type
    * as the test expression.
    */
   node->test->ty = check_expr (node->test, stack);

   if (!is_ty_kind (node->test->ty, TY_BOOLEAN) &&
       !is_ty_kind (node->test->ty, TY_INTEGER) &&
       !is_ty_kind (node->test->ty, TY_STRING))
   {
      TYPE_ERROR (cconfig.filename, node->test->lineno,
                  node->test->column, "test expression is not a basic type",
                  "test-expr", ty_to_str (node->test->ty), "expected",
                  L"boolean, integer, or string");
      exit(1);
   }

   for (tmp = node->branch_lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_branch_lst_t *b = tmp->data;

      /* Check the branch test-expr against the case's test-expr. */
      b->branch->ty = check_expr (b->branch, stack);

      if (!equal_types (node->test->ty, b->branch->ty))
      {
         TYPE_ERROR (cconfig.filename, b->branch->lineno, b->branch->column,
                     "branch test must have the same type as the test-expr",
                     "branch test", ty_to_str (node->test->ty), "test-expr",
                     ty_to_str (b->branch->ty));
         exit(1);
      }

      /* Check the branch's action expression. */
      b->expr->ty = check_expr (b->expr, stack);

      /* Check that all branch-exprs have the same type.  If this is the
       * first branch-expr, we can skip this test.
       */
      if (tmp->prev != NULL)
      {
         ty_t *prev_ty = ((absyn_branch_lst_t *) tmp->prev->data)->expr->ty;

         if (!equal_types (b->expr->ty, prev_ty))
         {
            TYPE_ERROR (cconfig.filename, b->expr->lineno, b->expr->column,
                        "inconsistent types in case branch exprs",
                        "previous expr", ty_to_str (prev_ty), "this expr",
                        ty_to_str (b->expr->ty));
            exit(1);
         }
      }

      /* Keep the entire case-expr's type updated so we don't have to think
       * about it later.
       */
      node->ty = b->expr->ty;
   }

   /* If there is a default expression, make sure it has the same type as all
    * the previous expressions.  If could also be the only branch, so take
    * care of that possibility as well.
    */
   if (node->default_expr != NULL)
   {
      node->default_expr->ty = check_expr (node->default_expr, stack);

      /* If there are branches besides just the default, we need to make sure
       * the default has the same type as all the others.  If the default
       * branch is the only possibility, we need to use its type as the type of
       * the whole case-expr.
       */
      if (node->branch_lst != NULL)
      {
         if (!equal_types (node->ty, node->default_expr->ty))
         {
            TYPE_ERROR (cconfig.filename, node->default_expr->lineno,
                        node->default_expr->column,
                        "default expr type does not match branch-expr types",
                        "branch-expr", ty_to_str (node->ty),
                        "default-expr", ty_to_str (node->default_expr->ty));
            exit(1);
         }
      }
      else
         node->ty = node->default_expr->ty;
   }
   else
   {
      /* If there is no default expression, make sure all possible values
       * of the test are covered.  Since case-expr returns a value, we have
       * to make sure that a value is defined for all the possible inputs.
       * For integers and strings, we can just assume there's no way every
       * value is covered.  For booleans, we can't yet figure out if all
       * possibilities are covered (if the branch tests are identifiers
       * especially) so just throw a warning.
       */
      if (is_ty_kind (node->test->ty, TY_INTEGER) ||
          is_ty_kind (node->test->ty, TY_STRING))
      {
         NONEXHAUSTIVE_MATCH_ERROR (cconfig.filename, node->lineno,
                                    node->column);
         exit(1);
      }
      else
      {
         NONEXHAUSTIVE_MATCH_WARNING (cconfig.filename, node->lineno,
                                      node->column);
         WARNINGS_AS_ERRORS();
      }
   }

   return node->ty;
}

static ty_t *check_decl_expr (absyn_decl_expr_t *node, tabstack_t *stack)
{
   stack = enter_scope (stack);
   check_decl_lst (node->decl_lst, stack);
   node->ty = check_expr (node->expr, stack);
   stack = leave_scope (stack, L"decl-expr");

   return node->ty;
}

static void process_fun_block (list_t *start, list_t *end, tabstack_t *stack)
{
   element_t   *tmp_ele;
   list_t      *tmp, *ast_tmp;

   /* Round 1:  Add skeleton entries for these functions so they can
    * mutually call each other.  A skeleton entry for a function consists of
    * the LHS of the declaration.  This is a little different than a type or
    * value in that we already know everything about the typing for the
    * function from looking at just the LHS.
    */
   for (tmp = start; tmp != end; tmp = tmp->next)
   {
      absyn_fun_decl_t *fun_decl = ((absyn_decl_t *) tmp->data)->fun_decl;
      absyn_id_expr_t  *fun_name = fun_decl->symbol;
      symbol_t         *new_sym = NULL;
      list_t           *formals = NULL;

      /* Add the function's symbol to the outer scope, since the function may
       * be referenced from within its own body.
       */
      MALLOC(new_sym, sizeof(symbol_t));
      MALLOC(new_sym->info.function, sizeof(function_symbol_t));

      new_sym->kind = SYM_FUNCTION;
      new_sym->name = fun_name->symbol;
      new_sym->info.function->retval = ast_to_ty (fun_decl->retval, stack);
      new_sym->info.function->formals = NULL;

      /* Build an unsorted list of all the formal parameters.  Formals need to
       * be stored in the order seen so we can match them against the actual
       * parameters at function call time and check type against type.
       */
      for (ast_tmp = fun_decl->formals; ast_tmp != NULL;
           ast_tmp = ast_tmp->next)
      {
         absyn_id_lst_t *formals_ast = ast_tmp->data;

         MALLOC (tmp_ele, sizeof (element_t));
         tmp_ele->identifier = formals_ast->symbol->symbol;
         tmp_ele->ty = ast_to_ty (formals_ast->ty, stack);

         formals = list_append (formals, tmp_ele);
      }

      /* Link the list of formal parameters into the symbol. */
      new_sym->info.function->formals = formals;

      /* Here's where we check for a duplicate symbol - don't have to
       * do this in check_fun_decl.
       */
      if (symtab_add_entry (stack, new_sym) == -1)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, fun_decl->lineno,
                           fun_decl->column, fun_name->symbol,
                           "duplicate symbol already exists in this scope");
         exit(1);
      }
   }

   /* Round 2:  Add full entries for each function, overwriting the skeletons
    * we made in round 1.
    */
   for (tmp = start; tmp != end; tmp = tmp->next)
      check_fun_decl (((absyn_decl_t *) tmp->data)->fun_decl, stack);
}

static void process_ty_block (list_t *start, list_t *end, tabstack_t *stack)
{
   list_t *tmp;

   /* Round 1:  Add skeleton entries for these types so they can mutually refer
    * to each other.
    */
   for (tmp = start ; tmp != end ; tmp = tmp->next)
   {
      absyn_id_expr_t *ty_sym = ((absyn_decl_t *) tmp->data)->ty_decl->symbol;
      symbol_t *new_sym = NULL;

      /* You're not allowed to make a type that overrides anything in the global
       * scope, since that scope contains our base types.
       */
      if (lookup_id (ty_sym, SYM_TYPE, global) != NULL ||
          lookup_id (ty_sym, SYM_EXN, global) != NULL)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, ty_sym->lineno,
                           ty_sym->column, ty_sym->symbol,
                           "duplicate symbol exists in global scope");
         exit(1);
      }

      /* Skeleton entries have a NULL ty pointer, which will be a magic
       * value later on indicating the entry can be overwritten.  Since we
       * don't yet know what's on the right hand side, we can't tell whether
       * to set kind to SYM_EXN or SYM_TYPE.  Just use SYM_TYPE for now and
       * have check_ty_decl change it as appropriate.
       */
      MALLOC (new_sym, sizeof(symbol_t));
      new_sym->kind = SYM_TYPE;
      new_sym->name = ty_sym->symbol;
      new_sym->info.ty = NULL;

      /* Here's where we check for a duplicate symbol - don't have to
       * do this in check_ty_decl.
       */
      if (symtab_add_entry (stack, new_sym) == -1)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, ty_sym->lineno,
                           ty_sym->column, ty_sym->symbol,
                           "duplicate symbol already exists in this scope");
         exit(1);
      }
   }

   /* Round 2:  Add full entries for each type, overwriting the skeletons we
    * made in round 1.
    */
   for (tmp = start; tmp != end; tmp = tmp->next)
      check_ty_decl (((absyn_decl_t *) tmp->data)->ty_decl, stack);

   /* Round 3: Now that we have types completely filled in, make sure none of
    * the type definitions form infinite loops.
    */
   for (tmp = start; tmp != end; tmp = tmp->next)
   {
      absyn_ty_decl_t *ty_decl = ((absyn_decl_t *) tmp->data)->ty_decl;

      if (ty_is_finite (ty_decl->ty) == TY_NOT_FINITE)
      {
         TYPE_LOOP_ERROR (cconfig.filename, ty_decl->lineno,
                          ty_decl->column, ty_decl->symbol->symbol);
         exit(1);
      }
   }
}

static void check_decl_lst (list_t *lst, tabstack_t *stack)
{
   list_t *start, *end;
   list_t *tmp = lst;

   while (tmp != NULL)
   {
      absyn_decl_t *decl = tmp->data;

      switch (decl->type) {
         case ABSYN_FUN_DECL:
            /* Set the bounds of this function block to [start, end) */
            start = end = tmp;
            while (end != NULL &&
                   ((absyn_decl_t *) end->data)->type == ABSYN_FUN_DECL)
               end = tmp = tmp->next;

            process_fun_block (start, end, stack);
            break;

         case ABSYN_MODULE_DECL:
            check_module_decl (decl->module_decl, stack);
            tmp = tmp->next;
            break;

         case ABSYN_TY_DECL:
            /* Set the bounds of this type block to [start, end) */
            start = end = tmp;
            while (end != NULL &&
                   ((absyn_decl_t *) end->data)->type == ABSYN_TY_DECL)
               end = tmp = end->next;

            process_ty_block (start, end, stack);
            break;

         case ABSYN_VAL_DECL:
            check_val_decl (decl->val_decl, stack);
            tmp = tmp->next;
            break;

#ifndef NEW_GRAMMAR
         default:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad decl->type");
            exit(1);
#endif
      }
   }
}

static ty_t *check_exn_expr (absyn_exn_expr_t *node, tabstack_t *stack)
{
   symbol_t *sym;
   ty_t *values_ty;

   /* Look up the type of node->identifier to make sure it's an exception. */
   if ((sym = lookup_id_global (node->identifier, SYM_EXN, stack)) == NULL)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->identifier->symbol, "unknown symbol referenced");
      exit(1);
   }

   /* Exceptions and types are in the same namespace. */
   if (sym->kind != SYM_EXN)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->identifier->symbol, "symbol is not an exception");
      exit(1);
   }

   /* Type check node->values as an absyn_record_assn_t.  We have to change the
    * return value, since it's going to be a TY_RECORD and we really want a
    * TY_EXN for equal_types to work.
    */
   values_ty = check_record_assn (node->values, stack);
   values_ty->ty = TY_EXN;

   /* Make sure the values in node->values match the list in the returned
    * type from earlier.
    */
   if (!equal_types (sym->info.ty, values_ty))
   {
      TYPE_ERROR (cconfig.filename, node->lineno, node->column,
                  "type of stated exn does not match list of assignments",
                  "exn type", ty_to_str (sym->info.ty),
                  "assignment list", ty_to_str (values_ty));
      exit(1);
   }

   node->ty = sym->info.ty;
   return sym->info.ty;
}

static ty_t *check_exn_handler (absyn_exn_handler_t *node, tabstack_t *stack)
{
   list_t *tmp;

   /* Check each handler in the list.  The types of all handler exprs must be
    * the same, as this will be the type of the exception handler in general.
    */
   for (tmp = node->handler_lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_exn_lst_t *e = tmp->data;

      if (node->ty == NULL)
      {
         node->ty = check_exn_lst (e, stack);
      }
      else
      {
         ty_t *tmp = check_exn_lst (e, stack);

         if (!equal_types (node->ty, tmp))
         {
            TYPE_ERROR (cconfig.filename, e->lineno, e->column,
                        "inconsistent types in exn handlers",
                        "first exn handler", ty_to_str (node->ty),
                        "this exn handler", ty_to_str (tmp));
            exit(1);
         }
      }
   }

   /* If there is a default handler, make sure it has the same type as all the
    * previous exception handlers.  It could also be the only handler, so take
    * care of that possibility as well.
    */
   if (node->default_handler != NULL)
   {
      if (node->handler_lst != NULL)
      {
         node->default_handler->ty = check_exn_lst (node->default_handler,
                                                    stack);

         if (!equal_types (node->ty, node->default_handler->ty))
         {
            TYPE_ERROR (cconfig.filename, node->default_handler->lineno,
                        node->default_handler->column,
                        "default exn handler does not match previous handlers",
                        "first exn handler", ty_to_str (node->ty),
                        "default exn handler",
                        ty_to_str (node->default_handler->ty));
            exit(1);
         }
      }
      else
         node->ty = check_exn_lst (node->default_handler, stack);
   }

   return node->ty;
}

static ty_t *check_exn_lst (absyn_exn_lst_t *node, tabstack_t *stack)
{
   symbol_t *sym, *new_sym;

   /* First, look up the type of the exn_id.  Default exception handlers have
    * no exn_id since they are bound to no particular exn, so watch out for
    * that.
    */
   if (node->exn_id != NULL)
   {
      if ((sym = lookup_id_global (node->exn_id, SYM_EXN, stack)) == NULL)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                           node->exn_id->symbol, "unknown symbol referenced");
         exit(1);
      }

      /* Exceptions and types are in the same namespace. */
      if (sym->kind != SYM_EXN)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                           node->exn_id->symbol, "symbol is not an exception");
         exit(1);
      }
   }
   else
      sym = NULL;

   /* Create a new environment, augmented with a binding for the id and the
    * type we just looked up.
    */
   MALLOC (new_sym, sizeof (symbol_t));

   new_sym->kind = SYM_EXN;
   new_sym->name = node->id;
   new_sym->info.ty = sym == NULL ? NULL : sym->info.ty;

   stack = enter_scope (stack);

   if (symtab_add_entry (stack, new_sym) == -1)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column, node->id,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }

   /* Check the type of the handler expression in that new environment. */
   node->ty = node->expr->ty = check_expr (node->expr, stack);
   
   stack = leave_scope (stack, L"exn-handler");
   return node->ty;
}

static ty_t *check_expr (absyn_expr_t *node, tabstack_t *stack)
{
   switch (node->kind) {
      case ABSYN_BOOLEAN:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_BOOLEAN;
         break;

      case ABSYN_BOTTOM:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_BOTTOM;
         break;

      case ABSYN_CASE:
         node->ty = check_case_expr (node->case_expr, stack);
         break;

      case ABSYN_DECL:
         node->ty = check_decl_expr (node->decl_expr, stack);
         break;

      case ABSYN_EXN:
         node->ty = check_exn_expr (node->exn_expr, stack);
         break;

      case ABSYN_EXPR_LST:
         node->ty = check_expr_lst (node->expr_lst, stack);
         break;

      case ABSYN_FUN_CALL:
         node->ty = check_fun_call (node->fun_call_expr, stack);
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

      case ABSYN_RAISE:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_ANY;
         check_expr (node->raise_expr, stack);
         break;

      case ABSYN_RECORD_ASSN:
         node->ty = check_record_assn (node->record_assn_lst, stack);
         break;

      case ABSYN_RECORD_REF:
         node->ty = check_record_ref (node->record_ref, stack);
         break;

      case ABSYN_STRING:
         MALLOC (node->ty, sizeof (ty_t));
         node->ty->ty = TY_STRING;
         break;

#ifndef NEW_GRAMMAR
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, "bad node->kind");
         exit(1);
#endif
   }

   /* If there's an exception handler attached to this expression, it better
    * have the same type as the expression itself.
    */
   if (node->exn_handler != NULL)
   {
      node->exn_handler->ty = check_exn_handler (node->exn_handler, stack);

      if (!equal_types (node->ty, node->exn_handler->ty))
      {
         TYPE_ERROR (cconfig.filename, node->exn_handler->lineno,
                     node->exn_handler->column,
                     "type of exn handler does not match type of expression",
                     "expr", ty_to_str (node->ty), "exn handler",
                     ty_to_str (node->exn_handler->ty));
         exit(1);
      }

      node->ty = node->exn_handler->ty;
   }

   return node->ty;
}

static ty_t *check_expr_lst (list_t *lst, tabstack_t *stack)
{
   list_t *tmp;
   ty_t *expr_ty = NULL;
   ty_t *retval;

   /* Check that each expression in the list has the same type as the first
    * expression in the list.
    */
   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_expr_t *node = tmp->data;

      if (expr_ty == NULL)
         expr_ty = node->ty = check_expr (node, stack);
      else
      {
         node->ty = check_expr (node, stack);

         if (!equal_types (node->ty, expr_ty))
         {
            TYPE_ERROR (cconfig.filename, node->lineno, node->column,
                        "inconsistent types in expression list",
                        "previous expr", ty_to_str (expr_ty),
                        "this expr", ty_to_str (node->ty));
            exit(1);
         }
      }
   }

   /* Now that we've verified all the expressions have the same type, create
    * a list type with the expr type as the base.
    */
   MALLOC (retval, sizeof (ty_t));
   retval->ty = TY_LIST;
   retval->list_base_ty = expr_ty;

   return retval;
}

static ty_t *check_fun_call (absyn_fun_call_t *node, tabstack_t *stack)
{
   symbol_t *s;
   list_t *arg_lst, *formal_lst;

   if ((s = lookup_id_global (node->identifier, SYM_FUNCTION, stack)) == NULL)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->identifier->symbol, "unknown symbol referenced");
      exit(1);
   }

   /* Values and functions exist in the same namespace, so check what we got. */
   if (s->kind != SYM_FUNCTION)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->identifier->symbol, "symbol is not a function");
      exit(1);
   }

   if (s->info.function == NULL)
   {
      MITCHELL_INTERNAL_ERROR (cconfig.filename, 
                               "referenced symbol has no type information");
      fprintf (stderr, "referenced symbol: %ls\n", node->identifier->symbol);
      exit(1);
   }

   /* Check that each argument's type matches against the corresponding formal
    * parameter's type.
    */
   arg_lst = node->arg_lst;
   formal_lst = s->info.function->formals;

   while (1)
   {
      absyn_expr_t *arg;
      element_t *ele;

      /* If both lists are at NULL, they were the same length and passed all
       * the other tests, so return success.  If only one list is at NULL, they
       * weren't the same length so fail.
       */
      if (arg_lst == NULL && formal_lst == NULL)
         break;
      else if (!(arg_lst != NULL && formal_lst != NULL))
      {
         ERROR_IN_FILE (cconfig.filename, node->lineno, node->column,
                        "number of actual parameters does not match number "
                        "of formals");
         exit(1);
      }

      /* If the argument does not have the same type as the formal in the
       * same position, fail.  Otherwise, we cycle around to the next one.
       * Also, we need to check the types of each actual parameter as we go
       * along.
       */
      ele = (element_t *) formal_lst->data;
      arg = (absyn_expr_t *) arg_lst->data;

      arg->ty = check_expr (arg, stack);

      if (!equal_types (arg->ty, ele->ty))
      {
         TYPE_ERROR (cconfig.filename, arg->lineno, arg->column,
                     "type of actual parameter does not match type of formal",
                     "actual parameter", ty_to_str (arg->ty),
                     "formal parameter", ty_to_str (ele->ty));
         exit(1);
      }

      arg_lst = arg_lst->next;
      formal_lst = formal_lst->next;
   }

   /* If it passes, set node->ty to the return type of the function. */
   node->ty = s->info.function->retval;
   return s->info.function->retval;
}

/* Check a function declaration and add that new type information into the
 * symbol table.  The type information and formal list was already added in by
 * check_decl.  We just need to enter the formal parameters into the
 * environment and check the function body against what we already know.
 */
static void check_fun_decl (absyn_fun_decl_t *node, tabstack_t *stack)
{
   list_t      *formals_ast;
   symbol_t    *fun_sym, *tmp_sym;
   list_t      *formals = NULL;
   ty_t        *body_ty;

   /* Look up the symbol entry for the function.  This entry represents the
    * entire LHS for the symbol.
    */
   if ((fun_sym = lookup_id (node->symbol, SYM_FUNCTION, stack)) == NULL)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->symbol->symbol, "unknown symbol referenced");
      exit(1);
   }

   if (fun_sym->info.function == NULL)
   {
      MITCHELL_INTERNAL_ERROR (cconfig.filename, 
                               "referenced symbol has no type information");
      fprintf (stderr, "referenced symbol: %ls\n", node->symbol->symbol);
      exit(1);
   }

   /* Add all the formal parameters to the inner scope of the function itself
    * since they don't make sense outside.  We constructed the symbol's
    * formal parameter list in the same order as the AST represented them, so
    * we can reference the AST for printing out debugging messages.
    */
   stack = enter_scope (stack);

   formals_ast = node->formals;
   formals = fun_sym->info.function->formals;

   while (formals != NULL)
   {
      MALLOC (tmp_sym, sizeof(symbol_t));
      tmp_sym->kind = SYM_VALUE;
      tmp_sym->name = ((element_t *) formals->data)->identifier;
      tmp_sym->info.ty = ((element_t *) formals->data)->ty;

      if (symtab_add_entry (stack, tmp_sym) == -1)
      {
         absyn_id_lst_t *id_lst = (absyn_id_lst_t *) formals_ast->data;

         BAD_SYMBOL_ERROR (cconfig.filename, id_lst->lineno, id_lst->column,
                           id_lst->symbol->symbol,
                           "duplicate formal parameter already exists");
         exit(1);
      }

      formals = formals->next;
      formals_ast = formals_ast->next;
   }

   /* Now check the function body within this augmented environment. */
   body_ty = check_expr (node->body, stack);

   /* Check that the body's return value matches with what the stated return
    * type was.
    */
   if (!equal_types (body_ty, fun_sym->info.function->retval))
   {
      TYPE_ERROR (cconfig.filename, node->lineno, node->column,
                  "type of function body does not match declared type",
                  "declared", ty_to_str (fun_sym->info.function->retval),
                  "body", ty_to_str (body_ty));
      exit(1);
   }

   stack = leave_scope (stack, node->symbol->symbol);
}

/* Look up only simple value identifiers - those which are not functions or
 * record references.  Those are handled elsewhere, which significantly
 * simplifies this function.
 */
static ty_t *check_id (absyn_id_expr_t *node, tabstack_t *stack)
{
   symbol_t *sym;

   if ((sym = lookup_id_global (node, SYM_VALUE, stack)) == NULL)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->symbol, "unknown symbol referenced");
      exit(1);
   }

   /* Values and functions exist in the same namespace, so check what we got. */
   if (sym->kind != SYM_VALUE)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->symbol, "symbol is not a value");
      exit(1);
   }

   return sym->info.ty;
}

static ty_t *check_if_expr (absyn_if_expr_t *node, tabstack_t *stack)
{
   node->test_expr->ty = check_expr (node->test_expr, stack);
   node->then_expr->ty = check_expr (node->then_expr, stack);
   node->else_expr->ty = check_expr (node->else_expr, stack);

   if (!is_ty_kind (node->test_expr->ty, TY_BOOLEAN))
   {
      TYPE_ERROR (cconfig.filename, node->test_expr->lineno,
                  node->test_expr->column,
                  "if-expr test must return bolean type", "if-expr",
                  ty_to_str (node->test_expr->ty), "expected", L"boolean");
      exit(1);
   }

   if (!equal_types (node->then_expr->ty, node->else_expr->ty))
   {
      TYPE_ERROR (cconfig.filename, node->else_expr->lineno,
                  node->else_expr->column,
                  "then-expr and else-expr must have the same type",
                  "then-expr", ty_to_str (node->then_expr->ty),
                  "else-expr", ty_to_str (node->else_expr->ty));
      exit(1);
   }

   return node->then_expr->ty;
}

static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack)
{
   symbol_t *new_sym;

   if (node->symbol->sub != NULL)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->symbol->symbol, "name may not contain a dot");
      exit(1);
   }

   MALLOC (new_sym, sizeof (symbol_t));

   /* Build symtab entry in lexical parent's table for this module. */
   new_sym->kind = SYM_MODULE;
   new_sym->name = node->symbol->symbol;
   new_sym->info.stack = enter_scope (new_sym->info.stack);
   
   /* Add the module's symbol table entry, with its pointer to initialized
    * inner symbol table.
    */
   if (symtab_add_entry (stack, new_sym) == -1)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->symbol->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }

   /* Check the guts of the module against the module's new environment. */
   check_decl_lst (node->decl_lst, new_sym->info.stack);

   /* Print out the symbol tables, if we're supposed to. */
   if (cconfig.debug.dump_symtab)
   {
      if (cconfig.debug.symtab_outfile == NULL ||
          strcmp ("-", cconfig.debug.symtab_outfile) == 0)
         symtab_dump (stdout, new_sym->info.stack, node->symbol->symbol);
      else
      {
         FILE *out;
         
         if ((out = fopen (cconfig.debug.symtab_outfile, "a")) == NULL)
         {
            COULD_NOT_OPEN_ERROR (cconfig.debug.symtab_outfile, "writing");
            exit(1);
         }

         symtab_dump (out, new_sym->info.stack, node->symbol->symbol);
      }
   }
}

static void check_module_lst (list_t *lst, tabstack_t *stack)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
      check_module_decl ((absyn_module_decl_t *) tmp->data, stack);
}

/* Convert an expression consisting of assignment to elements of a record
 * into a record type.  This is useful for initializing record values and
 * returning from a function.
 */
static ty_t *check_record_assn (list_t *lst, tabstack_t *stack)
{
   ty_t *retval;
   list_t *tmp;

   MALLOC (retval, sizeof (ty_t));
   retval->ty = TY_RECORD;
   retval->elts = NULL;

   /* Loop over all assignments in the record expression. */
   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_record_assn_t *node = tmp->data;
      element_t *new_ele;

      MALLOC (new_ele, sizeof(element_t));
      new_ele->identifier = node->symbol->symbol;
      new_ele->ty = check_expr (node->expr, stack);

      retval->elts = list_insert_unique (retval->elts, new_ele,
                                         __ele_to_ele_cmp);
      if (retval->elts == NULL)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                           node->symbol->symbol, "duplicate symbol already "
                           "exists in this record type");
         exit(1);
      }
   }

   return retval;
}

static ty_t *check_record_ref (absyn_record_ref_t *node, tabstack_t *stack)
{
   ty_t *retval = NULL;
   ty_t *sym_ty = NULL;
   absyn_id_expr_t *tmp;

   /* Records may be defined values or values returned from a function call,
    * though they must be of a type defined in both the function's scope and
    * the scope of the call.  In the function call case, we still have to
    * type check the expression before we may do anything else.
    */
   switch (node->rec->kind) {
      case ABSYN_ID:
      {
         symbol_t *sym = NULL;
         absyn_id_expr_t *id = node->rec->identifier;

         if ((sym = lookup_id_global (id, SYM_VALUE, stack)) == NULL &&
             (sym = lookup_id_global (id, SYM_EXN, stack)) == NULL)
         {
            BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                              id->symbol,
                              "unknown symbol referenced in record expression");
            exit(1);
         }

         sym_ty = unalias (sym->info.ty);

         if (!is_ty_kind (sym_ty, TY_RECORD) && !is_ty_kind (sym_ty, TY_EXN))
         {
            BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                              sym->name, "symbol does not allow member access");
            exit(1);
         }

         break;
      }

      case ABSYN_FUN_CALL:
         sym_ty = unalias(check_fun_call (node->rec->fun_call_expr, stack));
         
         if (!is_ty_kind (sym_ty, TY_RECORD))
         {
            BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                              node->rec->fun_call_expr->identifier->symbol,
                              "symbol is not a record");
            exit(1);
         }
         break;

      /* All other cases are ruled out by the grammar, but this shuts up a gcc
       * warning.
       */
      default:
         break;
   }

   /* Loop over each element in the path given, since we could be accessing
    * elements which themselves are records.
    */
   for (tmp = node->element; tmp != NULL; tmp = tmp->sub)
   {
      /* Find the current element in the record symbol's list. */
      list_t *l = list_find (sym_ty->elts, tmp->symbol, __ele_to_str_cmp);

      if (l == NULL)
      {
         BAD_SYMBOL_ERROR (cconfig.filename, tmp->lineno, tmp->column,
                           tmp->symbol, "symbol is not a member of the record");
         exit(1);
      }

      /* Set retval to the type of that element, in case we're at the end. */
      retval = ((element_t *) l->data)->ty;
      sym_ty = unalias (retval);
   }

   return retval;
}

/* Check the right hand side of a type declaration and add that new type
 * information into the symbol table.  The skeleton entry representing the
 * left hand side was already added in by process_ty_block.  We just need to
 * overwrite it with complete type information.  Exceptions make this only
 * slightly more complicated.
 */
static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack)
{
   symbol_t        *new = NULL;
   absyn_id_expr_t *lhs = node->symbol;
   ty_t            *rhs = ast_to_ty (node->ty_decl, stack);
   subtable_t       sub = rhs->ty == TY_EXN ? SYM_EXN : SYM_TYPE;

   if (rhs == NULL)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        lhs->symbol, "type declaration has no right hand side");
      exit(1);
   }

   /* Make a new symbol from the pieces we've assembled. */
   MALLOC(new, sizeof(symbol_t));
   new->kind = sub;
   new->name = lhs->symbol;
   node->ty = new->info.ty = rhs;

   /* Now obliterate the skeleton entry for this symbol with the real thing.
    * Note that process_ty_block set everything to be a type, even things that
    * are really exceptions.
    */
   if (table_update_entry (stack->symtab, lhs->symbol, SYM_TYPE, new) != 1)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, lhs->lineno, lhs->column,
                        lhs->symbol, "duplicate symbol already exists in this "
                        "scope");
      exit(1);
   }
}

static void check_val_decl (absyn_val_decl_t *node, tabstack_t *stack)
{
   symbol_t *new_sym;
   ty_t *val_ty, *expr_ty;

   expr_ty = node->init->ty = check_expr (node->init, stack);

   /* An explicit type is not required on the val decl.  If no type is given,
    * the value has the type of the initializer expression and we don't need
    * to worry about checking (of course).  If a type is given, it must match
    * the type of the initializing expression.
    */
   if (node->ty_decl == NULL)
      val_ty = expr_ty;
   else
   {
      val_ty = ast_to_ty (node->ty_decl, stack);

      if (!equal_types (val_ty, expr_ty))
      {
         TYPE_ERROR (cconfig.filename, node->lineno, node->column,
                     "type of value initializer does not match value's type",
                     "declared", ty_to_str (val_ty), "initializer",
                     ty_to_str (expr_ty));
         exit(1);
      }
   }
   
   MALLOC (new_sym, sizeof (symbol_t));

   new_sym->kind = SYM_VALUE;
   new_sym->name = node->symbol->symbol;
   node->ty = new_sym->info.ty = val_ty;
   
   if (symtab_add_entry (stack, new_sym) == -1)
   {
      BAD_SYMBOL_ERROR (cconfig.filename, node->lineno, node->column,
                        node->symbol->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }
}

/* vim: set tags=../tags: */
