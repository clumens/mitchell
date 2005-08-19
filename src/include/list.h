/* Implements a doubly-linked list that can store any type of data.  The
 * list nodes do not need to be homogenous, as each node simply stores a
 * pointer to the data.
 *
 * $Id: list.h,v 1.5 2005/08/19 01:15:20 chris Exp $
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
#ifndef _LIST_H
#define _LIST_H 1

#ifdef __cplusplus
    extern "C" {
#endif

/* A node of the linked list. */
typedef struct list_t {
   void          *data;
   struct list_t *prev;
   struct list_t *next;
} list_t;

/* The type of the comparison function several functions require - must return
 * zero if list data is equal to user data.
 */
typedef int (*cmp_func_t)(void *, void *);

/* The type of the list traversal function callback. */
typedef void (*trav_func_t)(void *);

/* List manipulation functions. */
list_t *list_append (list_t *lst, void *data);
list_t *list_concat (list_t *lst_a, list_t *lst_b);
list_t *list_find (list_t *lst, void *user_data, cmp_func_t cmp_func);
void list_foreach (list_t *lst, trav_func_t trav_func);
list_t *list_insert_unique (list_t *lst, void *user_data, cmp_func_t cmp_func);
int list_is_empty (list_t *lst);
unsigned int list_length (list_t *lst);
list_t *list_prepend (list_t *lst, void *data);
list_t *list_remove (list_t *lst, void *user_data, cmp_func_t cmp_func);
list_t *list_remove_hd (list_t *lst);
list_t *list_remove_tl (list_t *lst);
list_t *list_reverse (list_t *lst);
list_t *list_tl (list_t *lst);

#ifdef __cplusplus
    }
#endif

#endif

/* vim: set tags=../tags: */
