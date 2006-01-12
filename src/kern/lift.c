/* Lift all functions up out of whatever scope they are defined in to the module-level scope.  This flattens out
 * the AST somewhat, eliminating nested functions.  The result of this is that the AST is much closer to what
 * machine languages can represent and what our IR will eventually be.  This pass must come after free value
 * analysis and after all other passes that create function nodes.  Last is a good place for it.
 * 
 * $Id: lift.c,v 1.1 2006/01/11 22:02:36 chris Exp $
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
#include <wchar.h>

#include "absyn.h"
#include "absyn_walk.h"
#include "basic_types.h"
#include "desugar.h"
#include "error.h"
#include "list.h"
#include "symtab.h"
#include "translate.h"

static list_t *lift_visit_decl_lst (absyn_funcs_t *funcs, list_t *lst, void **user_data);
static absyn_module_decl_t *lift_visit_module_decl (absyn_funcs_t *funcs, absyn_module_decl_t *node, void **user_data);

/* Entry point for this pass. */
ast_t *desugar_lift_functions (absyn_funcs_t *funcs, ast_t *ast)
{
   list_t *tmp;

   for (tmp = ast; tmp != NULL; tmp = tmp->next)
      tmp->data = funcs->visit_module_decl (funcs, tmp->data, NULL);

   return ast;
}

/* Initialization for this pass. */
absyn_funcs_t *init_lift_pass()
{
   absyn_funcs_t *retval = init_default_funcs();
   retval->visit_decl_lst = lift_visit_decl_lst;
   retval->visit_module_decl = lift_visit_module_decl;
   return retval;
}

/* +================================================================+
 * | UTILITY FUNCTIONS                                              |
 * +================================================================+
 */
static int __decl_to_str_cmp (void *lst_data, void *user_data)
{
   absyn_decl_t *decl = lst_data;

   if (decl->type != ABSYN_FUN_DECL)
      return 0;
   else
      return wcscmp (decl->fun_decl->symbol->symbol, (wchar_t *) user_data);
}

/* +================================================================+
 * | PASS-LOCAL AST WALKING FUNCTIONS                               |
 * +================================================================+
 */

static list_t *lift_visit_decl_lst (absyn_funcs_t *funcs, list_t *lst, void **user_data)
{
   list_t *tmp = lst;

   while (tmp != NULL)
   {
      absyn_decl_t *decl = tmp->data;

      switch (decl->type) {
         case ABSYN_FUN_DECL:
         {
            /* Only add this fun-decl to the module's decl-lst (passed in via user_data) if the function's parent
             * is not a module.  If the parent is a module, it's already at module-level scope and doesn't need
             * lifting.
             */
            backlink_t *parent = find_lexical_parent (decl->fun_decl->parent);

            decl->fun_decl->body = funcs->visit_decl_expr (funcs, decl->fun_decl->body, user_data);

            if (parent->kind != LINK_MODULE_DECL)
            {
               *user_data = list_append (*user_data, decl);

               /* Remove the fun-decl node from the decl-lst now that we've added it to the module's list. */
               tmp = tmp->next;
               fprintf (stderr, "list length before: %d\n", list_length(lst));
               lst = list_remove (lst, decl->fun_decl->symbol->symbol, __decl_to_str_cmp);
               fprintf (stderr, "list length after: %d\n", list_length(lst));
               continue;
            }
         }

         case ABSYN_MODULE_DECL:
            decl->module_decl = funcs->visit_module_decl (funcs, decl->module_decl, user_data);
            break;

         case ABSYN_TY_DECL:
            break;

         case ABSYN_VAL_DECL:
            decl->val_decl = funcs->visit_val_decl (funcs, decl->val_decl, user_data);
            break;

#ifndef NEW_GRAMMAR
         default:
            MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__, N_("New AST type node type not handled.\n"));
#endif
      }

      tmp = tmp->next;
   }

   return lst;
}

static absyn_module_decl_t *lift_visit_module_decl (absyn_funcs_t *funcs, absyn_module_decl_t *node, void **user_data)
{
   /* If we see a module-decl, create a new list to store all the functions inside the module.
    * Then check the module to round up all the fun-decls, raise them to the top level, and
    * restore any previous list to handle enclosing modules.
    */
   list_t *fun_decls = NULL;
   list_t *prev_fun_decls = NULL;
   list_t *func_tmp;

   if (user_data == NULL)
      prev_fun_decls = NULL;
   else
      prev_fun_decls = *user_data;

   node->decl_lst = funcs->visit_decl_lst (funcs, node->decl_lst, (void *) &fun_decls);

   /* Time to raise all the functions. */
   for (func_tmp = fun_decls; func_tmp != NULL; func_tmp = func_tmp->next)
   {
      absyn_fun_decl_t *func = func_tmp->data;
      
      node->decl_lst = list_append (node->decl_lst, func);
      func->parent = NULL;
   }

   if (user_data != NULL)
      *user_data = prev_fun_decls;

   return node;
}

/* vim: set tags=../tags: */
