/* Pretty printer for the abstract syntax tree.  Please note that beauty is
 * in the eye of the beholder when examining the output.
 *
 * $Id: absyn_printer.c,v 1.2 2004/10/25 19:13:51 chris Exp $
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

#include "absyn.h"
#include "basic_types.h"

/* More mutually recursive functions means more forward declarations. */
static void print_absyn_branch_lst_t (absyn_branch_lst_t *node,
                                      unsigned int indent);
static void print_absyn_case_expr_t (absyn_case_expr_t *node,
                                     unsigned int indent);
static void print_absyn_decl_t (absyn_decl_t *node, unsigned int indent);
static void print_absyn_decl_expr_t (absyn_decl_expr_t *node,
                                     unsigned int indent);
static void print_absyn_decl_lst_t (absyn_decl_lst_t *node,
                                    unsigned int indent);
static void print_absyn_expr_t (absyn_expr_t *node, unsigned int indent);
static void print_absyn_expr_lst_t (absyn_expr_lst_t *node,
                                    unsigned int indent);
static void print_absyn_fun_decl_t (absyn_fun_decl_t *node,
                                    unsigned int indent);
static void print_absyn_fun_proto_t (absyn_fun_proto_t *node,
                                     unsigned int indent);
static void print_absyn_id_expr_t (absyn_id_expr_t *node, unsigned int indent);
static void print_absyn_id_lst_t (absyn_id_lst_t *node, unsigned int indent);
static void print_absyn_if_expr_t (absyn_if_expr_t *node, unsigned int indent);
static void print_absyn_module_decl_t (absyn_module_decl_t *node,
                                       unsigned int indent);
static void print_absyn_module_lst_t (absyn_module_lst_t *node,
                                      unsigned int indent);
static void print_absyn_proto_t (absyn_proto_t *node, unsigned int indent);
static void print_absyn_proto_lst_t (absyn_proto_lst_t *node,
                                     unsigned int indent);
static void print_absyn_record_lst_t (absyn_record_lst_t *node,
                                      unsigned int indent);
static void print_absyn_ty_t (absyn_ty_t *node, unsigned int indent);
static void print_absyn_ty_decl_t (absyn_ty_decl_t *node,
                                   unsigned int indent);
static void print_absyn_val_decl_t (absyn_val_decl_t *node,
                                    unsigned int indent);
static void print_absyn_val_proto_t (absyn_val_proto_t *node,
                                     unsigned int indent);

void print_absyn (ast_t *ast)
{
   print_absyn_module_lst_t (ast, 0);
   printf ("\n");
}

static void print_absyn_branch_lst_t (absyn_branch_lst_t *node,
                                      unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*sbranch_lst_t", indent, "");
      print_absyn_expr_t ((absyn_expr_t*) node->branch, indent+1);
      print_absyn_expr_t ((absyn_expr_t*) node->expr, indent+1);
      print_absyn_branch_lst_t ((absyn_branch_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_case_expr_t (absyn_case_expr_t *node,
                                     unsigned int indent)
{
   printf ("\n%*scase_expr_t", indent, "");
   print_absyn_expr_t ((absyn_expr_t*) node->test, indent+1);
   print_absyn_branch_lst_t (node->branch_lst, indent+1);
}

static void print_absyn_decl_t (absyn_decl_t *node, unsigned int indent)
{
   printf ("\n%*sdecl_t", indent, "");

   switch (node->type) {
      case ABSYN_FUN_DECL:
         print_absyn_fun_decl_t (node->fun_decl, indent+1);
         break;

      case ABSYN_TY_DECL:
         print_absyn_ty_decl_t (node->ty_decl, indent+1);
         break;

      case ABSYN_VAL_DECL:
         print_absyn_val_decl_t (node->val_decl, indent+1);
         break;
   }
}

static void print_absyn_decl_expr_t (absyn_decl_expr_t *node,
                                     unsigned int indent)
{
   printf ("\n%*sdecl_expr_t", indent, "");
   print_absyn_decl_lst_t ((absyn_decl_lst_t *) node->decl_lst, indent+1);
   print_absyn_expr_t ((absyn_expr_t *) node->expr, indent+1);
}

static void print_absyn_decl_lst_t (absyn_decl_lst_t *node, unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*sdecl_lst_t", indent, "");
      print_absyn_decl_t (node->decl, indent+1);
      print_absyn_decl_lst_t ((absyn_decl_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_expr_t (absyn_expr_t *node, unsigned int indent)
{
   printf ("\n%*sexpr_t", indent, "");

   switch (node->type)
   {
      case ABSYN_BOOLEAN:
         printf ("\n%*sBOOLEAN(%s)", indent+1, "",
                 node->boolean_expr == 0 ? "f" : "t");
         break;

      case ABSYN_CASE:
         print_absyn_case_expr_t (node->case_expr, indent+1);
         break;

      case ABSYN_DECL:
         print_absyn_decl_expr_t (node->decl_expr, indent+1);
         break;

      case ABSYN_EXPR_LST:
         print_absyn_expr_lst_t ((absyn_expr_lst_t *) node->expr_lst, indent+1);
         break;

      case ABSYN_FUN_CALL:
         printf ("\n%*sfun_call_expr", indent+1, "");
         print_absyn_id_expr_t (node->fun_call_expr.identifier, indent+2);
         print_absyn_expr_lst_t
            ((absyn_expr_lst_t *) node->fun_call_expr.arg_lst, indent+2);
         break;

      case ABSYN_ID:
         print_absyn_id_expr_t (node->identifier, indent+1);
         break;

      case ABSYN_IF:
         print_absyn_if_expr_t (node->if_expr, indent+1);
         break;

      case ABSYN_INTEGER:
         printf ("\n%*sINTEGER(%li)", indent+1, "", node->integer_expr);
         break;

      case ABSYN_RECORD_LST:
         print_absyn_record_lst_t (node->record_assn_lst, indent+1);
         break;

      case ABSYN_STRING:
         printf ("\n%*sSTRING(%ls)", indent+1, "", node->string_expr);
         break;
   }
}

static void print_absyn_expr_lst_t (absyn_expr_lst_t *node,
                                    unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*sexpr_lst_t", indent, "");
      print_absyn_expr_t (node->expr, indent+1);
      print_absyn_expr_lst_t ((absyn_expr_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_fun_decl_t (absyn_fun_decl_t *node,
                                    unsigned int indent)
{
   printf ("\n%*sfun_decl_t", indent, "");
   print_absyn_fun_proto_t (node->proto, indent+1);
   print_absyn_expr_t (node->body, indent+1);
}

static void print_absyn_fun_proto_t (absyn_fun_proto_t *node,
                                     unsigned int indent)
{
   printf ("\n%*sfun_proto_t(%ls)", indent, "", node->symbol);
   print_absyn_ty_t (node->ty, indent+1);
   print_absyn_id_lst_t (node->id_lst, indent+1);
}

static void print_absyn_id_expr_t (absyn_id_expr_t *node, unsigned int indent)
{
   printf ("\n%*sid_expr_t(%ls)", indent, "", node->symbol);
   if (node->ns != NULL)
      print_absyn_id_expr_t ((absyn_id_expr_t *) node->ns, indent+1);
}

static void print_absyn_id_lst_t (absyn_id_lst_t *node, unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*sid_lst_t(%ls)", indent, "", node->symbol);
      print_absyn_ty_t ((absyn_ty_t *) node->ty, indent+1);
      print_absyn_id_lst_t ((absyn_id_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_if_expr_t (absyn_if_expr_t *node, unsigned int indent)
{
   printf ("\n%*sif_expr_t", indent, "");
   print_absyn_expr_t ((absyn_expr_t *) node->test_expr, indent+1);
   print_absyn_expr_t ((absyn_expr_t *) node->then_expr, indent+1);
   print_absyn_expr_t ((absyn_expr_t *) node->else_expr, indent+1);
}

static void print_absyn_module_decl_t (absyn_module_decl_t *node,
                                       unsigned int indent)
{
   printf ("\n%*smodule_decl_t(%ls)", indent, "", node->symbol);
   print_absyn_proto_lst_t ((absyn_proto_lst_t *) node->proto_lst, indent+1);
   print_absyn_decl_lst_t ((absyn_decl_lst_t *) node->decl_lst, indent+1);
}

static void print_absyn_module_lst_t (absyn_module_lst_t *node,
                                      unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*smodule_lst_t", indent, "");
      print_absyn_module_decl_t (node->module, indent+1);
      print_absyn_module_lst_t ((absyn_module_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_proto_t (absyn_proto_t *node, unsigned int indent)
{
   printf ("\n%*sproto_t", indent, "");
   
   switch (node->type) {
      case ABSYN_FUN_DECL:
         print_absyn_fun_proto_t (node->fun_proto, indent+1);
         break;

      case ABSYN_TY_DECL:
         print_absyn_id_expr_t (node->ty_proto, indent+1);
         break;

      case ABSYN_VAL_DECL:
         print_absyn_val_proto_t (node->val_proto, indent+1);
         break;
   }
}

static void print_absyn_proto_lst_t (absyn_proto_lst_t *node,
                                     unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*sproto_lst_t", indent, "");
      print_absyn_proto_t (node->proto, indent+1);
      print_absyn_proto_lst_t ((absyn_proto_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_record_lst_t (absyn_record_lst_t *node,
                                      unsigned int indent)
{
   if (node != NULL)
   {
      printf ("\n%*srecord_lst(%ls)", indent, "", node->symbol);
      print_absyn_expr_t ((absyn_expr_t *) node->expr, indent+1);
      print_absyn_record_lst_t ((absyn_record_lst_t *) node->next, indent+1);
   }
}

static void print_absyn_ty_t (absyn_ty_t *node, unsigned int indent)
{
   printf ("\n%*sty_t", indent, "");

   if (node->is_record == 1)
      print_absyn_id_lst_t (node->record, indent+1);
   else
      print_absyn_id_expr_t (node->identifier, indent+1);

   if (node->is_list == 1)
      printf (" LIST");
}

static void print_absyn_ty_decl_t (absyn_ty_decl_t *node, unsigned int indent)
{
   printf ("\n%*sty_decl_t", indent, "");
   print_absyn_id_expr_t (node->proto, indent+1);
   print_absyn_ty_t (node->ty, indent+1);
}

static void print_absyn_val_decl_t (absyn_val_decl_t *node, unsigned int indent)
{
   printf ("\n%*sval_decl_t", indent, "");
   print_absyn_val_proto_t (node->proto, indent+1);
   print_absyn_expr_t (node->init, indent+1);
}

static void print_absyn_val_proto_t (absyn_val_proto_t *node,
                                     unsigned int indent)
{
   printf ("\n%*sval_proto_t(%ls)", indent, "", node->symbol);
   print_absyn_ty_t (node->ty, indent+1);
}

/* vim: set tags=../tags: */
