/* Implements a doubly-linked list that can store any type of data.  The
 * list nodes do not need to be homogenous, as each node simply stores a
 * pointer to the data.
 *
 * $Id: list.h,v 1.1 2004/12/15 03:39:45 chris Exp $
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

typedef struct list_t {
   void          *data;
   struct list_t *prev;
   struct list_t *next;
} list_t;

list_t *list_append (list_t *lst, void *data);
list_t *list_prepend (list_t *lst, void *data);
list_t *list_insert_unique (list_t *lst, void *user_data,
                            int (*cmp_func)(void *, void *));
list_t *list_remove_hd (list_t *lst);
list_t *list_remove_tl (list_t *lst);
list_t *list_remove (list_t *lst, void *user_data,
                     int (*cmp_func)(void *, void *));
unsigned int list_length (list_t *lst);
int list_is_empty (list_t *lst);
list_t *list_tl (list_t *lst);
void list_foreach (list_t *lst, void (*trav_func)(void *));

#endif
