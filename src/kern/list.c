/* Generic linked list management code.  Implements a doubly-linked list
 * that can store any type of data.  The list nodes do not need to be
 * homogenous, as each node simply stores a pointer to the data.
 *
 * $Id: list.c,v 1.3 2005/01/08 21:28:09 chris Exp $
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
#include <stdlib.h>
#include <stdio.h>

#include "list.h"
#include "memory.h"

/* +=====================================================================+
 * | PRIVATE FUNCTIONS                                                   |
 * +=====================================================================+
 */

/* Find the tail element of the list and return it.  This is an internal
 * list function useful for list_append and anything else that needs quick
 * access to the list's tail.
 */
static list_t *__list_find_tl (list_t *lst)
{
   if (lst == NULL)
      return NULL;

   while (lst->next != NULL)
      lst = lst->next;

   return lst;
}

/* +=====================================================================+
 * | PUBLIC FUNCTIONS                                                    |
 * +=====================================================================+
 */

list_t *list_append (list_t *lst, void *data)
{
   list_t *node, *tl;

   MALLOC (node, sizeof(list_t));
   node->data = data;

   /* If the list is empty, make the new node its head.  Otherwise, append. */
   if (lst == NULL)
   {
      lst = node;
      lst->next = lst->prev = NULL;
   }
   else
   {
      tl = __list_find_tl (lst);

      tl->next = node;
      node->prev = tl;
      node->next = NULL;
   }

   return lst;
}

list_t *list_find (list_t *lst, void *user_data, cmp_func_t cmp_func)
{
   list_t *tmp = lst;

   /* Find the node with the user's data in it. */
   while (tmp != NULL && cmp_func (tmp->data, user_data) != 0)
      tmp = tmp->next;

   return tmp;
}

void list_foreach (list_t *lst, trav_func_t trav_func)
{
   list_t *tmp = lst;

   while (tmp != NULL)
   {
      trav_func(tmp->data);
      tmp = tmp->next;
   }
}

list_t *list_insert_unique (list_t *lst, void *user_data, cmp_func_t cmp_func)
{
   list_t *tmp = lst;
   list_t *new = NULL;
   int cmp;

   /* Find the proper place to add. */
   while (tmp != NULL)
   {
      cmp = cmp_func (tmp->data, user_data);

      if (cmp > 0)
      {
         MALLOC (new, sizeof(list_t));
         new->data = user_data;

         /* Add before the head? */
         if (tmp == lst)
            return list_prepend (lst, user_data);
         else
         {
            /* Add the node before this current one. */
            new->next = tmp;
            new->prev = tmp->prev;
            tmp->prev = new;
            new->prev->next = new;

            return lst;
         }
      }
      else if (cmp == 0)
         return NULL;
      else
         tmp = tmp->next;
   }

   /* If we got this far, all elements in the list are smaller than the new
    * one.
    */
   if (tmp == NULL)
      return list_append (lst, user_data);
   else
      return lst;
}

int list_is_empty (list_t *lst)
{
   if (lst == NULL) return 1;
   else return 0;
}

unsigned int list_length (list_t *lst)
{
   unsigned int len = 0;

   while (lst != NULL)
   {
      len++;
      lst = lst->next;
   }

   return len;
}

list_t *list_prepend (list_t *lst, void *data)
{
   list_t *tmp;

   MALLOC (tmp, sizeof(list_t));
   tmp->data = data;

   if (tmp == NULL)
      return NULL;

   /* If the list is empty, make the new node its head.  Otherwise, prepend. */
   if (lst == NULL)
   {
      lst = tmp;
      lst->next = lst->prev = NULL;
   }
   else
   {
      tmp->prev = NULL;
      tmp->next = lst;
      lst->prev = tmp;
   }

   return tmp;
}

list_t *list_remove (list_t *lst, void *user_data, cmp_func_t cmp_func)
{
   list_t *tmp = list_find (lst, user_data, cmp_func);

   if (tmp == NULL)
      return NULL;

   /* Handle the node being at the beginning of the list. */
   if (tmp == lst)
      lst = lst->next;

   /* Unlink the node from the list. */
   if (tmp->prev != NULL) tmp->prev->next = tmp->next;
   if (tmp->next != NULL) tmp->next->prev = tmp->prev;

   tmp->data = NULL;
   tmp = NULL;

   return lst;
}

list_t *list_remove_hd (list_t *lst)
{
   list_t *tmp = lst;

   /* If this is the only element in the list, this is easy. */
   if (lst != NULL && lst->next == NULL)
      return NULL;
   else if (lst != NULL)
   {
      lst = lst->next;
      if (lst != NULL) lst->prev = NULL;
      
      tmp->next = NULL;
      tmp->data = NULL;
      tmp = NULL;
   }

   return lst;
}

list_t *list_remove_tl (list_t *lst)
{
   list_t *tl = __list_find_tl(lst);

   /* If this is the only element in the list, this is easy. */
   if (tl == lst)
      return NULL;

   if (tl != NULL)
   {
      if (tl->prev != NULL)
         tl->prev->next = NULL;

      tl->data = NULL;
      tl = NULL;
   }

   return lst;
}

list_t *list_tl (list_t *lst)
{
   return __list_find_tl (lst);
}

/* vim: set tags=../tags: */
