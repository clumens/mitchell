/* Pretty printer for the abstract syntax tree.  Please note that beauty is
 * in the eye of the beholder when examining the output.
 *
 * $Id: absyn_printer.c,v 1.23 2005/01/22 01:04:15 chris Exp $
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#include "absyn.h"
#include "config.h"
#include "basic_types.h"
#include "error.h"
#include "list.h"

/* More mutually recursive functions means more forward declarations. */
static void print_branch_lst_t (list_t *lst, unsigned int indent);
static void print_case_expr_t (absyn_case_expr_t *node, unsigned int indent);
static void print_decl_t (absyn_decl_t *node, unsigned int indent);
static void print_decl_expr_t (absyn_decl_expr_t *node, unsigned int indent);
static void print_decl_lst (list_t *lst, unsigned int indent);
static void print_expr_t (absyn_expr_t *node, unsigned int indent);
static void print_expr_lst (list_t *lst, unsigned int indent);
static void print_fun_decl_t (absyn_fun_decl_t *node, unsigned int indent);
static void print_id_expr_t (absyn_id_expr_t *node, unsigned int indent);
static void print_id_lst_t (list_t *lst, unsigned int indent);
static void print_if_expr_t (absyn_if_expr_t *node, unsigned int indent);
static void print_module_decl_t (absyn_module_decl_t *node,
                                 unsigned int indent);
static void print_module_lst (list_t *lst, unsigned int indent);
static void print_record_assn_t (list_t *lst, unsigned int indent);
static void print_record_ref_t (absyn_record_ref_t *node, unsigned int indent);
static void print_ty_t (absyn_ty_t *node, unsigned int indent);
static void print_ty_decl_t (absyn_ty_decl_t *node, unsigned int indent);
static void print_val_decl_t (absyn_val_decl_t *node, unsigned int indent);

static FILE *out = NULL;

/* Abstract syntax tree printer entry point. */
void print_absyn (ast_t *ast, compiler_config_t *config)
{
   if (config->debug.absyn_outfile == NULL ||
       strcmp ("-", config->debug.absyn_outfile) == 0)
      out = stdout;
   else
   {
      if ((out = fopen (config->debug.absyn_outfile, "w")) == NULL)
      {
         COULD_NOT_OPEN_ERROR (config->debug.absyn_outfile, "writing");
         exit(1);
      }
   }

   print_module_lst (ast, 0);
   fprintf (out, "\n");

   if (config->debug.absyn_outfile != NULL)
   {
      if (fclose (out) != 0)
      {
         FCLOSE_ERROR (config->debug.absyn_outfile);
         exit(1);
      }
   }
}

/* +=================================================================+
 * | AST PRINTING FUNCTIONS - ONE PER NODE TYPE                      |
 * +=================================================================+
 */

static void print_branch_lst_t (list_t *lst, unsigned int indent)
{
   unsigned int local_indent = indent;
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_branch_lst_t *node = tmp->data;

      fprintf (out, "\n%*sbranch_lst_t", local_indent, "");
      print_expr_t (node->branch, local_indent+1);
      print_expr_t (node->expr, local_indent+1);
      local_indent++;
   }
}

static void print_case_expr_t (absyn_case_expr_t *node, unsigned int indent)
{
   fprintf (out, "\n%*scase_expr_t", indent, "");
   print_expr_t (node->test, indent+1);

   /* Case expressions with just a default don't have a branch list. */
   if (node->branch_lst != NULL)
      print_branch_lst_t (node->branch_lst, indent+1);
   
   /* Not all case expressions have a default. */
   if (node->default_expr != NULL)
      print_expr_t (node->default_expr, indent+1);
}

static void print_decl_t (absyn_decl_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sdecl_t", indent, "");

   switch (node->type) {
      case ABSYN_FUN_DECL:
         print_fun_decl_t (node->fun_decl, indent+1);
         break;

      case ABSYN_MODULE_DECL:
         print_module_decl_t (node->module_decl, indent+1);
         break;

      case ABSYN_TY_DECL:
         print_ty_decl_t (node->ty_decl, indent+1);
         break;

      case ABSYN_VAL_DECL:
         print_val_decl_t (node->val_decl, indent+1);
         break;
   }
}

static void print_decl_expr_t (absyn_decl_expr_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sdecl_expr_t", indent, "");
   print_decl_lst (node->decl_lst, indent+1);
   print_expr_t (node->expr, indent+1);
}

static void print_decl_lst (list_t *lst, unsigned int indent)
{
   unsigned int local_indent = indent;
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_decl_t *node = tmp->data;

      fprintf (out, "\n%*sdecl_lst_t", local_indent, "");
      print_decl_t (node, local_indent+1);
      local_indent++;
   }
}

static void print_expr_t (absyn_expr_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sexpr_t", indent, "");

   switch (node->kind)
   {
      case ABSYN_BOOLEAN:
         fprintf (out, "\n%*sBOOLEAN(%s)", indent+1, "",
                  node->boolean_expr == 0 ? "f" : "t");
         break;

      case ABSYN_BOTTOM:
         fprintf (out, "\n%*s⊥", indent+1, "");
         break;

      case ABSYN_CASE:
         print_case_expr_t (node->case_expr, indent+1);
         break;

      case ABSYN_DECL:
         print_decl_expr_t (node->decl_expr, indent+1);
         break;

      case ABSYN_EXPR_LST:
         print_expr_lst (node->expr_lst, indent+1);
         break;

      case ABSYN_FUN_CALL:
         fprintf (out, "\n%*sfun_call_expr", indent+1, "");
         print_id_expr_t (node->fun_call_expr->identifier, indent+2);
         print_expr_lst (node->fun_call_expr->arg_lst, indent+2);
         break;

      case ABSYN_ID:
         print_id_expr_t (node->identifier, indent+1);
         break;

      case ABSYN_IF:
         print_if_expr_t (node->if_expr, indent+1);
         break;

      case ABSYN_INTEGER:
         fprintf (out, "\n%*sINTEGER(%li)", indent+1, "", node->integer_expr);
         break;

      case ABSYN_RECORD_ASSN:
         print_record_assn_t (node->record_assn_lst, indent+1);
         break;

      case ABSYN_RECORD_REF:
         print_record_ref_t (node->record_ref, indent+1);
         break;

      case ABSYN_STRING:
         fprintf (out, "\n%*sSTRING(%ls)", indent+1, "", node->string_expr);
         break;
   }
}

static void print_expr_lst (list_t *lst, unsigned int indent)
{
   unsigned int local_indent = indent;
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_expr_t *node = tmp->data;

      fprintf (out, "\n%*sexpr_lst_t", local_indent, "");
      print_expr_t (node, local_indent+1);
      local_indent++;
   }
}

static void print_fun_decl_t (absyn_fun_decl_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sfun_decl_t", indent, "");
   print_id_expr_t (node->symbol, indent+1);
   print_ty_t (node->retval, indent+1);
   print_id_lst_t (node->formals, indent+1);
   print_expr_t (node->body, indent+1);
}

static void print_id_expr_t (absyn_id_expr_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sid_expr_t(%ls)", indent, "", node->symbol);
   if (node->sub != NULL)
      print_id_expr_t (node->sub, indent+1);
}

static void print_id_lst_t (list_t *lst, unsigned int indent)
{
   unsigned int local_indent = indent;
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_id_lst_t *id = tmp->data;

      fprintf (out, "\n%*sid_lst_t", local_indent, "");
      print_id_expr_t (id->symbol, local_indent+1);
      print_ty_t (id->ty, local_indent+1);
      local_indent++;
   }
}

static void print_if_expr_t (absyn_if_expr_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sif_expr_t", indent, "");
   print_expr_t (node->test_expr, indent+1);
   print_expr_t (node->then_expr, indent+1);
   print_expr_t (node->else_expr, indent+1);
}

static void print_module_decl_t (absyn_module_decl_t *node, unsigned int indent)
{
   fprintf (out, "\n%*smodule_decl_t", indent, "");
   print_id_expr_t (node->symbol, indent+1);
   print_decl_lst (node->decl_lst, indent+1);
}

static void print_module_lst (list_t *lst, unsigned int indent)
{
   unsigned int local_indent = indent;
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_module_decl_t *node = tmp->data;

      fprintf (out, "\n%*smodule_lst_t", local_indent, "");
      print_module_decl_t (node, local_indent+1);
      local_indent++;
   }
}

static void print_record_assn_t (list_t *lst, unsigned int indent)
{
   unsigned int local_indent = indent;
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_record_assn_t *node = tmp->data;

      fprintf (out, "\n%*srecord_lst", local_indent, "");
      print_id_expr_t (node->symbol, local_indent+1);
      print_expr_t (node->expr, local_indent+1);
      local_indent++;
   }
}

static void print_record_ref_t (absyn_record_ref_t *node, unsigned int indent)
{
   if (node != NULL)
   {
      fprintf (out, "\n%*srecord_ref", indent, "");
      print_expr_t (node->rec, indent+1);
      print_id_expr_t (node->element, indent+1);
   }
}

static void print_ty_t (absyn_ty_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sty_t", indent, "");

   switch (node->kind) {
      case ABSYN_TY_BOTTOM:
         fprintf (out, "⊥");
         break;

      case ABSYN_TY_ID:
         print_id_expr_t (node->identifier, indent+1);
         break;

      case ABSYN_TY_LIST:
         fprintf (out, "(list)");
         print_ty_t (node->list, indent+1);
         break;

      case ABSYN_TY_RECORD:
         print_id_lst_t (node->record, indent+1);
         break;
   }
}

static void print_ty_decl_t (absyn_ty_decl_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sty_decl_t", indent, "");
   print_id_expr_t (node->symbol, indent+1);
   print_ty_t (node->ty_decl, indent+1);
}

static void print_val_decl_t (absyn_val_decl_t *node, unsigned int indent)
{
   fprintf (out, "\n%*sval_decl_t", indent, "");
   print_id_expr_t (node->symbol, indent+1);
   print_ty_t (node->ty_decl, indent+1);
   print_expr_t (node->init, indent+1);
}

/* vim: set tags=../tags: */
