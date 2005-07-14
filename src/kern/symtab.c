/* Symbol table manipulation.
 *
 * $Id: symtab.c,v 1.25 2005/07/14 03:02:53 chris Exp $
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

#include "basic_types.h"
#include "config.h"
#include "error.h"
#include "memory.h"
#include "symtab.h"
#include "translate.h"

static inline unsigned int hash (const mstring_t *str, const subtable_t kind)
{
   unsigned int len = strlen ((char *) str);
   unsigned int g, h, i;

   if (kind == SYM_VALUE)
      h = SYM_FUNCTION;
   else if (kind == SYM_EXN)
      h = SYM_TYPE;
   else
      h = kind;

   for (i = 0; i < len; i++)
   {
      h = (h << 4) + (int) str[i];
      g = h & 0xf0000000;

      if (g != 0)
      {
         h ^= g >> 24;
         h ^= g;
      }
   }

   return h % SYMTAB_ROWS;
}

/* Our tables are a little overloaded with what's equal to what, so take that
 * into consideration.
 */
static inline unsigned int equal_kinds (const subtable_t a, const subtable_t b)
{
   if (((a == SYM_VALUE || a == SYM_FUNCTION) &&
        (b == SYM_VALUE || b == SYM_FUNCTION)) ||
       ((a == SYM_EXN || a == SYM_TYPE) && (b == SYM_EXN || b == SYM_TYPE)))
      return 1;
   else
      return a == b;
}

/* +================================================================+
 * | FUNCTIONS FOR MANIPULATING A SINGLE SYMBOL TABLE               |
 * +================================================================+
 */

/* Create a new symbol table, initializing all its buckets to NULL.  Return
 * a pointer to the new table.
 */
symtab_t *symtab_new ()
{
   symtab_t *retval;
   unsigned int i;

   MALLOC (retval, sizeof (symtab_t));

   for (i = 0; i < SYMTAB_ROWS; i++)
      (*retval)[i] = NULL;

   return retval;
}

/* Add a new entry into the symbol table, returning success or not. */
int table_add_entry (symtab_t *symtab, symbol_t *sym)
{
   symtab_entry_t *tmp;
   unsigned int row = hash(sym->name, sym->kind);

   if (symtab == NULL)
      return 0;

   if (table_entry_exists (symtab, sym->name, sym->kind) == 1)
      return -1;
   
   if ((*symtab)[row] == NULL)
   {
      MALLOC ((*symtab)[row], sizeof(symtab_entry_t));
      MALLOC ((*symtab)[row]->symbol, sizeof(symbol_t));

      (*symtab)[row]->symbol->kind = sym->kind;
      (*symtab)[row]->symbol->name = wcsdup (sym->name);

      if (sym->label != NULL)
         (*symtab)[row]->symbol->label = wcsdup (sym->label);
      else
         (*symtab)[row]->symbol->label = NULL;

      (*symtab)[row]->next = NULL;

      switch (sym->kind) {
         case SYM_MODULE:
            (*symtab)[row]->symbol->info.stack = sym->info.stack;
            break;

         case SYM_FUNCTION:
            (*symtab)[row]->symbol->info.function = sym->info.function;
            break;

         case SYM_EXN:
         case SYM_TYPE:
         case SYM_VALUE:
            (*symtab)[row]->symbol->info.ty = sym->info.ty;
            break;

         case SYM_NONE:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__,
                                     N_("Symbol has type of SYM_NONE: %ls\n"),
                                     sym->name);
      }
   }
   else
   {
      /* Scan to the end of the row, since we're not being clever and just
       * appending new entries.
       */
      tmp = (*symtab)[row];
      while (tmp->next != NULL)
         tmp = tmp->next;

      MALLOC (tmp->next, sizeof(symtab_entry_t));
      MALLOC (tmp->next->symbol, sizeof(symbol_t));

      tmp->next->symbol->kind = sym->kind;
      tmp->next->symbol->name = wcsdup (sym->name);

      if (sym->label != NULL)
         (*symtab)[row]->symbol->label = wcsdup (sym->label);
      else
         (*symtab)[row]->symbol->label = NULL;

      tmp->next->next = NULL;

      switch (sym->kind) {
         case SYM_MODULE:
            tmp->next->symbol->info.stack = sym->info.stack;
            break;

         case SYM_FUNCTION:
            tmp->next->symbol->info.function = sym->info.function;
            break;

         case SYM_EXN:
         case SYM_TYPE:
         case SYM_VALUE:
            tmp->next->symbol->info.ty = sym->info.ty;
            break;

         case SYM_NONE:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__,
                                     N_("Symbol has type of SYM_NONE: %ls\n"),
                                     sym->name);
      }
   }

   return 1;
}

/* Lookup an entry in the given symbol table. */
symbol_t *table_lookup_entry (symtab_t *symtab, mstring_t *name,
                              subtable_t kind)
{
   unsigned int row = hash (name, kind);
   symtab_entry_t *tmp;

   if (symtab == NULL || (*symtab)[row] == NULL)
      return NULL;

   for (tmp = (*symtab)[row] ; tmp != NULL ; tmp = tmp->next)
      if (wcscmp ((wchar_t *) name, (wchar_t *) tmp->symbol->name) == 0 &&
          equal_kinds (kind, tmp->symbol->kind))
         return tmp->symbol;

   return NULL;
}

/* Does the given string exist in the symbol table?  Return success or not. */
unsigned int table_entry_exists (symtab_t *symtab, mstring_t *name,
                                 subtable_t kind)
{
   return table_lookup_entry (symtab, name, kind) == NULL ? 0 : 1;
}

/* Search for a symbol with the given name and kind.  If it exists, remove it
 * and replace with the symbol new.  If it does not exist, simply add new into
 * the symbol table.
 */
int table_update_entry (symtab_t *symtab, mstring_t *name, subtable_t kind,
                        symbol_t *newsym)
{
   if (table_entry_exists (symtab, name, kind) == 0)
      return table_add_entry (symtab, newsym);
   else
   {
      unsigned int row    = hash (name, kind);
      symtab_entry_t *cur = (*symtab)[row];

      /* Scan until we get to the right entry - we know the entry exists
       * somewhere since table_entry_exists told us so and there's no
       * threading-type problems in this program.
       */
      while (1)
      {
         if (wcscmp (name, cur->symbol->name) == 0 &&
             equal_kinds (kind, cur->symbol->kind))
            break;
         else
            cur = cur->next;
      }

      /* cur now points at the entry we need to revise.  Overwite its
       * contents with that of the new data.  Also overwrite the kind field
       * since we're allowed to change that too.
       */
      cur->symbol->kind = newsym->kind;

      switch (newsym->kind) {
         case SYM_MODULE:
            cur->symbol->info.stack = newsym->info.stack;
            break;

         case SYM_FUNCTION:
            cur->symbol->info.function = newsym->info.function;
            break;

         case SYM_EXN:
         case SYM_TYPE:
         case SYM_VALUE:
            cur->symbol->info.ty = newsym->info.ty;
            break;

         case SYM_NONE:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__,
                                     N_("Symbol has type of SYM_NONE: %ls\n"),
                                     newsym->name);
      }
   }

   return 1;
}

/* +================================================================+
 * | FUNCTIONS FOR MANIPULATING A STACK OF SYMBOL TABLES            |
 * +================================================================+
 */

/* Enter a new level of scope by creating a new symbol table and putting it
 * on the top of the symbol table stack, ensuring it will be the first to be
 * referenced.  Returns a pointer to the new stack.
 */
tabstack_t *enter_scope (tabstack_t *tabstack)
{
   tabstack_t *tmp;

   if (tabstack == NULL)
   {
      MALLOC (tabstack, sizeof (tabstack_t));
      tabstack->symtab = symtab_new();
      tabstack->upper = NULL;
   }
   else
   {
      MALLOC (tmp, sizeof (tabstack_t));
      tmp->symtab = symtab_new();
      tmp->upper = tabstack;
      tabstack = tmp;
   }

   return tabstack;
}

/* Leave the innermost level of scope by popping the table off the top of the
 * stack and destroying it.  Returns a pointer to the new stack.
 */
tabstack_t *leave_scope (tabstack_t *tabstack, mstring_t *scope_name)
{
   /* Print out the symbol tables, if we're supposed to. */
   if (cconfig.debug.dump_symtab)
   {
      if (cconfig.debug.symtab_outfile == NULL ||
          strcmp ("-", cconfig.debug.symtab_outfile) == 0)
         table_dump (stdout, tabstack->symtab, scope_name);
      else
      {
         FILE *out;

         if ((out = fopen (cconfig.debug.symtab_outfile, "a")) == NULL)
         {
            COULD_NOT_WRITE_ERROR (cconfig.debug.symtab_outfile);
            exit(1);
         }

         table_dump (out, tabstack->symtab, scope_name);
      }
   }

   tabstack = tabstack->upper;
   return tabstack;
}

int symtab_add_entry (tabstack_t *tabstack, symbol_t *sym)
{
   return table_add_entry (tabstack->symtab, sym);
}

symbol_t *symtab_lookup_entry (tabstack_t *tabstack, mstring_t *name,
                               subtable_t kind)
{
   tabstack_t *tmp = tabstack;
   symbol_t *retval;

   while (tmp != NULL)
   {
      retval = table_lookup_entry (tmp->symtab, name, kind);
      if (retval != NULL)
         return retval;

      tmp = tmp->upper;
   }

   return NULL;
}

unsigned int symtab_entry_exists (tabstack_t *tabstack, mstring_t *name,
                                  subtable_t kind)
{
   return (symtab_lookup_entry (tabstack, name, kind) == NULL) ?
      0 : 1;
}

unsigned int symtab_entry_exists_local (tabstack_t *tabstack, mstring_t *name,
                                        subtable_t kind)
{
   return table_entry_exists (tabstack->symtab, name, kind);
}

/* vim: set tags=../tags: */
