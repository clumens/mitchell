/* Semantic analysis - symbol table management, type checking, and so forth.
 * Let's hope this goes better than my previous efforts at semantic analysis
 * have.
 *
 * $Id: semant.c,v 1.19 2004/12/12 17:39:44 chris Exp $
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
static void check_decl_lst (absyn_decl_lst_t *node, tabstack_t *stack);
static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack);
static void check_module_lst (absyn_module_lst_t *node, tabstack_t *stack);
static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack);

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
    * strings must hatch the order of the type enumeration in basic_types.h.
    */
   static wchar_t *ty_map[] = {
      L"alias", L"boolean", L"bottom", L"integer", L"list", L"record",
      L"string" };

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

      case TY_RECORD:
      {
         wchar_t *retval, *tmp;
         element_lst_t *ele;

         /* First, the brace indicating a record type. */
         MALLOC(retval, sizeof(wchar_t)*2);
         retval = wcscpy (retval, L"{");

         for (ele = ty->record; ele != NULL; ele = ele->next)
         {
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
            REALLOC(retval, WCSLEN(retval)+sizeof(wchar_t)*2);
            retval = wcscat (retval, L",");
         }

         /* Finally, the closing brace. */
         REALLOC(retval, WCSLEN(retval)+sizeof(wchar_t)*6);
         retval = wcscat (retval, L"NULL}");

         return retval;
         break;
      }

      default:
         return NULL;
         break;
   }
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */

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

            /* Really, this should never happen (kiss of death, I know). */
            assert (entry != NULL);
            assert (entry->info.stack != NULL);
            assert (entry->info.stack->symtab != NULL);

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

/* Convert an AST representation of a type into a corresponding symbol table
 * type declaration, suitable for inserting into tables.
 */
static ty_t *ast_to_ty (absyn_ty_t *node, tabstack_t *stack)
{
   ty_t *retval = NULL;

   switch (node->kind) {
      case ABSYN_TY_ID:
      {
         /* First check the local symbol table stack (to take into account any
          * modules we might be inside of).  If that fails, also check the
          * global symbol table for those basic types.
          */
         symbol_t *s;

         if ((s = lookup_id (node->identifier, SYM_TYPE, stack)) == NULL)
         {
            if ((s = lookup_id (node->identifier, SYM_TYPE, global)) == NULL)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                                 node->identifier->symbol,
                                 "unknown symbol referenced");
               exit(1);
            }
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

      case ABSYN_TY_RECORD:
      {
         absyn_id_lst_t *cur_id, *tmp_id;
         element_lst_t *new_ele, *cur_ele = NULL;

         MALLOC(retval, sizeof(ty_t));
         retval->ty = TY_RECORD;
         retval->record = NULL;

         for (cur_id = node->record; cur_id != NULL; cur_id = cur_id->next)
         {
            /* First, make sure there's no other record member with the same
             * name.
             */
            for (tmp_id = cur_id->next; tmp_id != NULL; tmp_id = tmp_id->next)
            {
               if (wcscmp (tmp_id->symbol->symbol, cur_id->symbol->symbol) == 0)
               {
                  BAD_SYMBOL_ERROR (compiler_config.filename, tmp_id->lineno,
                                    tmp_id->symbol->symbol, "duplicate symbol "
                                    "already exists in this record type");
                  exit(1);
               }
            }

            /* Now do all the hard work of adding a record element entry. */
            MALLOC(new_ele, sizeof(element_lst_t));
            new_ele->identifier = wcsdup(cur_id->symbol->symbol);
            new_ele->ty = ast_to_ty (cur_id->ty, stack);
            new_ele->next = NULL;

            /* Link the new_ele entry into place. */
            if (retval->record == NULL)
            {
               retval->record = new_ele;
               cur_ele = new_ele;
            }
            else
            {
               cur_ele->next = new_ele;
               cur_ele = cur_ele->next;
            }
         }

         return retval;
         break;
      }
   }

   return retval;
}

/* +================================================================+
 * | TYPE CHECKING FUNCTIONS - ONE PER AST NODE TYPE                |
 * +================================================================+
 */

static void check_decl_lst (absyn_decl_lst_t *node, tabstack_t *stack)
{
   absyn_decl_lst_t *tmp;

   /* Round 1:  Add skeleton entries for types, and do modules completely. */
   for (tmp = node; tmp != NULL; tmp = tmp->next)
   {
      switch (tmp->decl->type) {
         case ABSYN_MODULE_DECL:
            check_module_decl (tmp->decl->module_decl, stack);
            break;

         case ABSYN_TY_DECL:
         {
            absyn_id_expr_t *sym = tmp->decl->ty_decl->symbol;
            symbol_t *new_sym = NULL;

            /* Skeleton entries have a NULL ty pointer, which will be a magic
             * value later on indicating the entry can be overwritten.
             */
            MALLOC (new_sym, sizeof(symbol_t));
            new_sym->kind = SYM_TYPE;
            new_sym->name = wcsdup (sym->symbol);
            new_sym->info.ty = NULL;

            /* Here's where we check for a duplicate symbol - don't have to
             * do this in check_ty_decl.
             */
            if (symtab_add_entry (stack, new_sym) == -1)
            {
               BAD_SYMBOL_ERROR (compiler_config.filename, sym->lineno,
                                 sym->symbol, "duplicate symbol already "
                                 "exists in this scope");
               exit(1);
            }

            break;
         }

         case ABSYN_FUN_DECL:
         case ABSYN_VAL_DECL:
            break;
      }
   }

   /* Round 2:  Add full entries for each type, overwriting the skeletons
    * we made in round 1.  We also need to hook up ty pointers in the AST for
    * these things, so aliased types work.  Skip modules.
    */
   for (tmp = node; tmp != NULL; tmp = tmp->next)
   {
      switch (tmp->decl->type) {
         case ABSYN_TY_DECL:
            check_ty_decl (tmp->decl->ty_decl, stack);
            break;

         case ABSYN_FUN_DECL:
         case ABSYN_MODULE_DECL:
         case ABSYN_VAL_DECL:
            break;
      }
   }
}

static void check_module_decl (absyn_module_decl_t *node, tabstack_t *stack)
{
   symbol_t *new_sym;

   if (node->symbol->sub != NULL)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                        node->symbol->symbol, "name may not contain a dot");
      exit(1);
   }

   MALLOC (new_sym, sizeof (symbol_t));

   /* Build symtab entry in lexical parent's table for this module. */
   new_sym->kind = SYM_MODULE;
   new_sym->name = wcsdup (node->symbol->symbol);
   new_sym->info.stack = enter_scope (new_sym->info.stack);
   
   /* Add the module's symbol table entry, with its pointer to initialized
    * inner symbol table.
    */
   if (symtab_add_entry (stack, new_sym) == -1)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno,
                        node->symbol->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }

   /* Check the guts of the module against the module's new environment. */
   check_decl_lst (node->decl_lst, new_sym->info.stack);

   if (compiler_config.debug.dump_symtabs)
      symtab_dump (new_sym->info.stack, node->symbol->symbol);
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

/* Check the right hand side of a type declaration and add that new type
 * information into the symbol table.  The skeleton entry representing the
 * left hand side was already added in by check_decl.  We just need to
 * overwrite it with complete type information.
 */
static void check_ty_decl (absyn_ty_decl_t *node, tabstack_t *stack)
{
   symbol_t        *new = NULL;
   absyn_id_expr_t *lhs = node->symbol;
   ty_t            *rhs = ast_to_ty (node->ty, stack);

   if (rhs == NULL)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, node->lineno, lhs->symbol,
                        "type declaration has no right hand side");
      exit(1);
   }

   /* Make a new symbol from the pieces we've assembled. */
   MALLOC(new, sizeof(symbol_t));
   new->kind = SYM_TYPE;
   new->name = wcsdup (lhs->symbol);
   new->info.ty = rhs;

   /* Now obliterate the skeleton entry for this symbol with the real thing. */
   if (table_update_entry (stack->symtab, lhs->symbol, SYM_TYPE, new) != 1)
   {
      BAD_SYMBOL_ERROR (compiler_config.filename, lhs->lineno, lhs->symbol,
                        "duplicate symbol already exists in this scope");
      exit(1);
   }
}

/* vim: set tags=../tags: */
