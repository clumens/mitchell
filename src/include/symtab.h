/* This file defines the structures that make up symbol tables.  We are using
 * a hash table for each symbol table, and a stack of those tables to represent
 * lexical scope.  The topmost table on the stack represents the current level
 * of scope we're at, and will be the first to be examined.  This will also be
 * the table where most new symbols will be added.  Leaving a level of scope
 * corresponds to removing this topmost table from the stack.
 *
 * $Id: symtab.h,v 1.2 2004/11/18 03:59:54 chris Exp $
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

#define SYMTAB_ROWS  47

/* Our one big symbol table actually contains three different name spaces.
 * This enum is how we tell them apart, and this value will get added in with
 * the symbol name in the hash function so three symbols with the same name can
 * still coexist.
 */
typedef enum { SYM_FUNVAL, SYM_MODULE, SYM_TYPE } subtable_t;

typedef struct symbol_t {
   subtable_t kind;
   mstring_t *name;
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

/* Manipulation functions for a single symbol table.  These probably don't
 * need to be used by anyone.
 */
symtab_t *symtab_new ();
int table_add_entry (symtab_t *symtab, symbol_t *symbol);
void table_dump (symtab_t *symtab);
unsigned int table_entry_exists (symtab_t *symtab, symbol_t *symbol);

/* Symbol table nesting for scope. */
typedef struct tabstack_t {
   symtab_t *symtab;             /* current symbol table */
   struct tabstack_t *upper;     /* link to next table up in scope */
} tabstack_t;

/* Functions for manipulating an entire stack of symbol tables.  These should
 * be used by client code for symbol table management, rather than the single
 * functions above.
 */
tabstack_t *enter_scope (tabstack_t *tabstack);
tabstack_t *leave_scope (tabstack_t *tabstack);
int symtab_add_entry (tabstack_t *tabstack, symbol_t *symbol);
void symtab_dump (tabstack_t *tabstack);
unsigned int symtab_entry_exists (tabstack_t *tabstack, symbol_t *symbol);
unsigned int symtab_entry_exists_local (tabstack_t *tabstack, symbol_t *symbol);

#endif

/* vim: set tags=../tags: */
