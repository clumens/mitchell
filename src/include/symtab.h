/* This file defines the structures that make up symbol tables.  We are using
 * a hash table for each symbol table, and a stack of those tables to represent
 * lexical scope.  The topmost table on the stack represents the current level
 * of scope we're at, and will be the first to be examined.  This will also be
 * the table where most new symbols will be added.  Leaving a level of scope
 * corresponds to removing this topmost table from the stack.
 *
 * $Id: symtab.h,v 1.14 2005/04/20 22:51:57 chris Exp $
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
#ifndef _SYMTAB_H
#define _SYMTAB_H 1

#include <wchar.h>

#include "basic_types.h"
#include "list.h"

/* +================================================================+
 * | SYMBOL TABLE TYPES                                             |
 * +================================================================+
 */

#define SYMTAB_ROWS  47

/* Our one big symbol table actually contains three different name spaces.
 * This enum is how we tell them apart, and this value will get added in with
 * the symbol name in the hash function so three symbols with the same name can
 * still coexist.  Note:  SYM_FUNCTION and SYM_VALUE are different enum values,
 * but must be handled the same by the hashing function.  Same with SYM_EXN and
 * SYM_TYPE.
 */
typedef enum { SYM_EXN, SYM_FUNCTION, SYM_MODULE, SYM_TYPE,
               SYM_VALUE } subtable_t;

typedef struct {
   struct ty_t *retval;
   list_t      *formals;
} function_symbol_t;

typedef struct symbol_t {
   subtable_t   kind;
   mstring_t   *name, *label;

   union {
      function_symbol_t *function;        /* SYM_FUNCTION */
      struct tabstack_t *stack;           /* SYM_MODULE */
      struct ty_t       *ty;              /* SYM_EXN, SYM_TYPE, SYM_VALUE */
   } info;
} symbol_t;

/* A symbol table entry. */
typedef struct symtab_entry_t {
   symbol_t *symbol;
   struct symtab_entry_t *next;
} symtab_entry_t;

/* A hash table with chaining - an array of pointers to linked lists of
 * entries.
 */
typedef symtab_entry_t *symtab_t[SYMTAB_ROWS];

/* Symbol table nesting for scope. */
typedef struct tabstack_t {
   symtab_t *symtab;             /* current symbol table */
   struct tabstack_t *upper;     /* link to next table up in scope */
} tabstack_t;

/* +================================================================+
 * | TYPE CHECKER TYPES                                             |
 * +================================================================+
 */

/* These are obvious except for a couple:
 *    TY_ALIAS is when you make a new name for an existing type,
 *    TY_ANY is the type of a raise-expr, since it needs to be equal to
 *           whatever type is expected for type checking purposes
 *    TY_BOTTOM is the type for when there's no other type (⊥)
 */
typedef enum { TY_ALIAS, TY_ANY, TY_BOOLEAN, TY_BOTTOM, TY_EXN, TY_INTEGER,
               TY_LIST, TY_RECORD, TY_STRING } ty_kind;

/* Used for determining whether a type is part of an infinite loop or not. */
typedef enum { TY_NOT_FINITE, TY_UNVISITED, TY_VISITED, TY_FINITE } ty_finite;

typedef struct {
   mstring_t *identifier;
   struct ty_t *ty;
} element_t;

typedef struct ty_t {
   ty_kind ty;
   ty_finite is_finite;

   union {
      symbol_t      *alias;               /* TY_ALIAS */
      struct ty_t   *list_base_ty;        /* TY_LIST */
      list_t        *elts;                /* TY_EXN, TY_RECORD - list of
                                             element_t */
   };
} ty_t;

/* +================================================================+
 * | FUNCTIONS                                                      |
 * +================================================================+
 */

/* Functions for manipulating a single symbol table. */
symbol_t *table_lookup_entry (symtab_t *symtab, mstring_t *name,
                              subtable_t kind);
symtab_t *symtab_new();
int table_add_entry (symtab_t *symtab, symbol_t *symbol);
unsigned int table_entry_exists (symtab_t *symtab, mstring_t *name,
                                 subtable_t kind);
int table_update_entry (symtab_t *symtab, mstring_t *name, subtable_t kind,
                        symbol_t *newsym);

/* Functions for manipulating an entire stack of symbol tables.  These should
 * be used by client code for symbol table management, rather than the single
 * functions above.
 */
tabstack_t *enter_scope (tabstack_t *tabstack);
tabstack_t *leave_scope (tabstack_t *tabstack, mstring_t *scope_name);
int symtab_add_entry (tabstack_t *tabstack, symbol_t *symbol);
symbol_t *symtab_lookup_entry (tabstack_t *tabstack, mstring_t *name,
                               subtable_t kind);
unsigned int symtab_entry_exists (tabstack_t *tabstack, mstring_t *name,
                                  subtable_t kind);
unsigned int symtab_entry_exists_local (tabstack_t *tabstack, mstring_t *name,
                                        subtable_t kind);

/* Functions to dump the symbol tables. */
void symbol_dump (FILE *f, symbol_t *sym);
void symtab_dump (FILE *f, tabstack_t *tabstack, mstring_t *scope_name);
void table_dump (FILE *f, symtab_t *symtab, mstring_t *scope_name);

#endif

/* vim: set tags=../tags: */
