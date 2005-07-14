/* Pretty printer for the abstract syntax tree.  Please note that beauty is
 * in the eye of the beholder when examining the output.
 *
 * $Id: absyn_printer.c,v 1.36 2005/07/13 23:35:59 chris Exp $
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
#include "translate.h"

/* More mutually recursive functions means more forward declarations. */
static void print_branch_lst_t (list_t *lst, unsigned int il);
static void print_case_expr_t (absyn_case_expr_t *node, unsigned int il);
static void print_decl_t (absyn_decl_t *node, unsigned int il);
static void print_decl_expr_t (absyn_decl_expr_t *node, unsigned int il);
static void print_decl_lst (list_t *lst, unsigned int il);
static void print_exn_expr_t (absyn_exn_expr_t *node, unsigned int il);
static void print_exn_handler_t (absyn_exn_handler_t *node, unsigned int il);
static void print_exn_lst_t (absyn_exn_lst_t *lst, unsigned int il);
static void print_expr_t (absyn_expr_t *node, unsigned int il);
static void print_expr_lst (list_t *lst, unsigned int il);
static void print_fun_call_t (absyn_fun_call_t *node, unsigned int il);
static void print_fun_decl_t (absyn_fun_decl_t *node, unsigned int il);
static void print_id_expr_t (absyn_id_expr_t *node, unsigned int il);
static void print_id_lst_t (list_t *lst, unsigned int il);
static void print_if_expr_t (absyn_if_expr_t *node, unsigned int il);
static void print_module_decl_t (absyn_module_decl_t *node, unsigned int il);
static void print_module_lst (list_t *lst, unsigned int il);
static void print_record_assn_t (list_t *lst, unsigned int il);
static void print_record_ref_t (absyn_record_ref_t *node, unsigned int il);
static void print_ty_t (absyn_ty_t *node, unsigned int il);
static void print_ty_decl_t (absyn_ty_decl_t *node, unsigned int il);
static void print_val_decl_t (absyn_val_decl_t *node, unsigned int il);

static FILE *out = NULL;

/* Abstract syntax tree printer entry point. */
void print_absyn (ast_t *ast, compiler_config_t *config, char *header)
{
   if (config->debug.absyn_outfile == NULL ||
       strcmp ("-", config->debug.absyn_outfile) == 0)
      out = stdout;
   else
   {
      if ((out = fopen (config->debug.absyn_outfile, "w")) == NULL)
      {
         COULD_NOT_WRITE_ERROR (config->debug.absyn_outfile);
         exit(1);
      }
   }

   fprintf (out, "\n%s\n========================================", header);
   print_module_lst (ast, 0);
   fprintf (out, "\n");

   if (config->debug.absyn_outfile != NULL)
   {
      if (out != stdout && fclose (out) != 0)
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

/* How much do we really want to indent? */
#define INDENT(i) (i)*3

static void print_branch_lst_t (list_t *lst, unsigned int il)
{
   list_t *tmp;

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_branch_lst_t *node = tmp->data;

      fprintf (out, "\n%*sbranch_lst_t = {", INDENT(il), "");

      fprintf (out, "\n%*sbranch =", INDENT(il+1), "");
      print_expr_t (node->branch, il+2);

      fprintf (out, "\n%*sexpr =", INDENT(il+1), "");
      print_expr_t (node->expr, il+2);

      fprintf (out, "\n%*s},", INDENT(il), "");
   }
}

static void print_case_expr_t (absyn_case_expr_t *node, unsigned int il)
{
   fprintf (out, "\n%*scase_expr_t = {", INDENT(il), "");

   fprintf (out, "\n%*stest =", INDENT(il+1), "");
   print_expr_t (node->test, il+2);

   /* Case expressions with just a default don't have a branch list. */
   if (node->branch_lst != NULL)
   {
      fprintf (out, "\n%*sbranch_lst = [", INDENT(il+1), "");
      print_branch_lst_t (node->branch_lst, il+2);
      fprintf (out, " NULL\n%*s]", INDENT(il+1), "");
   }
   else
      fprintf (out, "\n%*sbranch_lst = [ NULL ]", INDENT(il+1), "");
   
   /* Not all case expressions have a default. */
   if (node->default_expr != NULL)
   {
      fprintf (out, "\n%*sdefault_expr =", INDENT(il+1), "");
      print_expr_t (node->default_expr, il+2);
   }
   else
      fprintf (out, "\n%*sdefault_expr = [ NULL ]", INDENT(il+1), "");

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_decl_t (absyn_decl_t *node, unsigned int il)
{
   fprintf (out, "\n%*sdecl_t =", INDENT(il), "");

   switch (node->type) {
      case ABSYN_FUN_DECL:
         print_fun_decl_t (node->fun_decl, il+1);
         break;

      case ABSYN_MODULE_DECL:
         print_module_decl_t (node->module_decl, il+1);
         break;

      case ABSYN_TY_DECL:
         print_ty_decl_t (node->ty_decl, il+1);
         break;

      case ABSYN_VAL_DECL:
         print_val_decl_t (node->val_decl, il+1);
         break;

#ifndef NEW_GRAMMAR
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, _("bad node->type"),
                                  __FILE__, __LINE__);
         exit(1);
#endif
   }
}

static void print_decl_expr_t (absyn_decl_expr_t *node, unsigned int il)
{
   fprintf (out, "\n%*sdecl_expr_t = {", INDENT(il), "");
   fprintf (out, "\n%*ssymtab = %p", INDENT(il+1), "", node->symtab);

   fprintf (out, "\n%*sdecl_lst =", INDENT(il+1), "");
   print_decl_lst (node->decl_lst, il+2);

   fprintf (out, "\n%*sexpr =", INDENT(il+1), "");
   print_expr_t (node->expr, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_decl_lst (list_t *lst, unsigned int il)
{
   list_t *tmp;

   fprintf (out, "\n%*sdecl_lst_t = [", INDENT(il), "");

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_decl_t *node = tmp->data;

      print_decl_t (node, il+1);
      fprintf (out, ",");
   }

   fprintf (out, " NULL\n%*s]", INDENT(il), "");
}

static void print_exn_expr_t (absyn_exn_expr_t *node, unsigned int il)
{
   fprintf (out, "\n%*sexn_expr_t = {", INDENT(il), "");

   fprintf (out, "\n%*sidentifier =", INDENT(il+1), "");
   print_id_expr_t (node->identifier, il+2);

   fprintf (out, "\n%*svalues =", INDENT(il+1), "");
   print_record_assn_t (node->values, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_exn_handler_t (absyn_exn_handler_t *node, unsigned int il)
{
   fprintf (out, "\n%*sexn_handler_t = {", INDENT(il), "");

   if (node->handler_lst != NULL)
   {
      list_t *tmp;

      fprintf (out, "\n%*shandler_lst = [", INDENT(il+1), "");

      for (tmp = node->handler_lst; tmp != NULL; tmp = tmp->next)
      {
         print_exn_lst_t (tmp->data, il+2);
         fprintf (out, ",");
      }

      fprintf (out, " NULL\n%*s]", INDENT(il+1), "");
   }
   
   /* Not all handler lists have a default. */
   if (node->default_handler != NULL)
   {
      fprintf (out, "\n%*sdefault_handler = {", INDENT(il+1), "");
      print_exn_lst_t (node->default_handler, il+2);
      fprintf (out, "\n%*s}", INDENT(il+1), "");
   }
   else
      fprintf (out, "\n%*sdefault_handler = NULL", INDENT(il+1), "");

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_exn_lst_t (absyn_exn_lst_t *node, unsigned int il)
{
   fprintf (out, "\n%*sexn_lst_t = {", INDENT(il), "");
   fprintf (out, "\n%*ssymtab = %p", INDENT(il+1), "", node->symtab);

   if (node->exn_id != NULL)
   {
      fprintf (out, "\n%*sexn_id =", INDENT(il+1), "");
      print_id_expr_t (node->exn_id, il+2);
   }
   else
      fprintf (out, "\n%*sexn_id = NULL", INDENT(il+1), "");

   fprintf (out, "\n%*sid =\n%*sSTRING(%ls)", INDENT(il+1), "", INDENT(il+2),
            "", node->id);

   fprintf (out, "\n%*sexpr =", INDENT(il+1), "");
   print_expr_t (node->expr, il+2);
   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_expr_t (absyn_expr_t *node, unsigned int il)
{
   fprintf (out, "\n%*sexpr_t =", INDENT(il), "");

   switch (node->kind)
   {
      case ABSYN_BOOLEAN:
         fprintf (out, "\n%*sBOOLEAN(%s)", INDENT(il+1), "",
                  node->boolean_expr == 0 ? "f" : "t");
         break;

      case ABSYN_BOTTOM:
         fprintf (out, "\n%*s⊥", il+1, "");
         break;

      case ABSYN_CASE:
         print_case_expr_t (node->case_expr, il+1);
         break;

      case ABSYN_DECL:
         print_decl_expr_t (node->decl_expr, il+1);
         break;

      case ABSYN_EXN:
         print_exn_expr_t (node->exn_expr, il+1);
         break;

      case ABSYN_EXPR_LST:
         print_expr_lst (node->expr_lst, il+1);
         break;

      case ABSYN_FUN_CALL:
         print_fun_call_t (node->fun_call_expr, il+1);
         break;

      case ABSYN_ID:
         print_id_expr_t (node->identifier, il+1);
         break;

      case ABSYN_IF:
         print_if_expr_t (node->if_expr, il+1);
         break;

      case ABSYN_INTEGER:
         fprintf (out, "\n%*sINTEGER(%li)", INDENT(il+1), "",
                  node->integer_expr);
         break;

      case ABSYN_RAISE:
         fprintf (out, "\n%*sraise_expr =", INDENT(il+1), "");
         print_expr_t (node->raise_expr, il+2);
         break;

      case ABSYN_RECORD_ASSN:
         print_record_assn_t (node->record_assn_lst, il+1);
         break;

      case ABSYN_RECORD_REF:
         print_record_ref_t (node->record_ref, il+1);
         break;

      case ABSYN_STRING:
         fprintf (out, "\n%*sSTRING(%ls)", INDENT(il+1), "", node->string_expr);
         break;

#ifndef NEW_GRAMMAR
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, _("bad node->kind"), 
                                  __FILE__, __LINE__);
         exit(1);
#endif
   }

   if (node->exn_handler != NULL)
      print_exn_handler_t (node->exn_handler, il+1);
}

static void print_expr_lst (list_t *lst, unsigned int il)
{
   list_t *tmp;

   fprintf (out, "\n%*sexpr_lst_t = [", INDENT(il), "");

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_expr_t *node = tmp->data;

      print_expr_t (node, il+1);
      fprintf (out, ",");
   }

   fprintf (out, " NULL\n%*s]", INDENT(il), "");
}

static void print_fun_call_t (absyn_fun_call_t *node, unsigned int il)
{
   fprintf (out, "\n%*sfun_call_expr = {", INDENT(il), "");

   fprintf (out, "\n%*sidentifier =", INDENT(il+1), "");
   print_id_expr_t (node->identifier, il+2);

   fprintf (out, "\n%*sarg_lst =", INDENT(il+1), "");
   print_expr_lst (node->arg_lst, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_fun_decl_t (absyn_fun_decl_t *node, unsigned int il)
{
   fprintf (out, "\n%*sfun_decl_t = {", INDENT(il), "");
   fprintf (out, "\n%*ssymtab = %p", INDENT(il+1), "", node->symtab);

   fprintf (out, "\n%*ssymbol =", INDENT(il+1), "");
   print_id_expr_t (node->symbol, il+2);
   
   fprintf (out, "\n%*sretval =", INDENT(il+1), "");
   print_ty_t (node->retval, il+2);
   
   fprintf (out, "\n%*sformals =", INDENT(il+1), "");
   print_id_lst_t (node->formals, il+2);
   
   fprintf (out, "\n%*sbody =", INDENT(il+1), "");
   print_decl_expr_t (node->body, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_id_expr_t (absyn_id_expr_t *node, unsigned int il)
{
   fprintf (out, "\n%*sid_expr_t(%ls, %ls)", INDENT(il), "", node->symbol,
                 node->label);
   if (node->sub != NULL)
      print_id_expr_t (node->sub, il+1);
}

static void print_id_lst_t (list_t *lst, unsigned int il)
{
   list_t *tmp;

   fprintf (out, "\n%*sid_lst_t = [", INDENT(il), "");

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_id_lst_t *id = tmp->data;

      fprintf (out, "\n%*ssymbol =", INDENT(il+1), "");
      print_id_expr_t (id->symbol, il+2);

      fprintf (out, "\n%*sty =", INDENT(il+1), "");
      print_ty_t (id->ty, il+2);

      fprintf (out, ",");
   }

   fprintf (out, " NULL\n%*s]", INDENT(il), "");
}

static void print_if_expr_t (absyn_if_expr_t *node, unsigned int il)
{
   fprintf (out, "\n%*sif_expr_t = {", INDENT(il), "");

   fprintf (out, "\n%*stest_expr =", INDENT(il+1), "");
   print_expr_t (node->test_expr, il+2);

   fprintf (out, "\n%*sthen_expr =", INDENT(il+1), "");
   print_expr_t (node->then_expr, il+2);

   fprintf (out, "\n%*selse_expr =", INDENT(il+1), "");
   print_expr_t (node->else_expr, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_module_decl_t (absyn_module_decl_t *node, unsigned int il)
{
   fprintf (out, "\n%*smodule_decl_t = {", INDENT(il), "");
   fprintf (out, "\n%*ssymtab = %p", INDENT(il+1), "", node->symtab);

   fprintf (out, "\n%*ssymbol =", INDENT(il+1), "");
   print_id_expr_t (node->symbol, il+2);

   fprintf (out, "\n%*sdecl_lst =", INDENT(il+1), "");
   print_decl_lst (node->decl_lst, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_module_lst (list_t *lst, unsigned int il)
{
   list_t *tmp;

   fprintf (out, "\n%*smodule_lst_t = [", INDENT(il), "");

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_module_decl_t *node = tmp->data;

      print_module_decl_t (node, il+1);
      fprintf (out, ",");
   }

   fprintf (out, " NULL\n%*s]", INDENT(il), "");
}

static void print_record_assn_t (list_t *lst, unsigned int il)
{
   list_t *tmp;

   fprintf (out, "\n%*srecord_lst = [", INDENT(il), "");

   for (tmp = lst; tmp != NULL; tmp = tmp->next)
   {
      absyn_record_assn_t *node = tmp->data;

      print_id_expr_t (node->symbol, il+1);
      print_expr_t (node->expr, il+1);
      fprintf (out, ",");
   }

   fprintf (out, " NULL\n%*s]", INDENT(il), "");
}

static void print_record_ref_t (absyn_record_ref_t *node, unsigned int il)
{
   if (node == NULL)
      return;

   fprintf (out, "\n%*srecord_ref = {", INDENT(il), "");

   fprintf (out, "\n%*srec =", INDENT(il+1), "");
   print_expr_t (node->rec, il+2);

   fprintf (out, "\n%*selement =", INDENT(il+1), "");
   print_id_expr_t (node->element, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_ty_t (absyn_ty_t *node, unsigned int il)
{
   if (node == NULL)
      return;

   fprintf (out, "\n%*sty_t =", INDENT(il), "");

   switch (node->kind) {
      case ABSYN_TY_BOTTOM:
         fprintf (out, "⊥");
         break;

      case ABSYN_TY_EXN:
         fprintf (out, " EXN");
         print_id_lst_t (node->exn, il+1);
         break;

      case ABSYN_TY_ID:
         print_id_expr_t (node->identifier, il+1);
         break;

      case ABSYN_TY_LIST:
         fprintf (out, " LIST");
         print_ty_t (node->list, il+1);
         break;

      case ABSYN_TY_RECORD:
         print_id_lst_t (node->record, il+1);
         break;

#ifndef NEW_GRAMMAR
      default:
         MITCHELL_INTERNAL_ERROR (cconfig.filename, _("bad node->kind"),
                                  __FILE__, __LINE__);
         exit(1);
#endif
   }
}

static void print_ty_decl_t (absyn_ty_decl_t *node, unsigned int il)
{
   fprintf (out, "\n%*sty_decl_t = {", INDENT(il), "");

   fprintf (out, "\n%*ssymbol =", INDENT(il+1), "");
   print_id_expr_t (node->symbol, il+2);

   fprintf (out, "\n%*sty_decl =", INDENT(il+1), "");
   print_ty_t (node->ty_decl, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

static void print_val_decl_t (absyn_val_decl_t *node, unsigned int il)
{
   fprintf (out, "\n%*sval_decl_t = {", INDENT(il), "");

   fprintf (out, "\n%*ssymbol =", INDENT(il+1), "");
   print_id_expr_t (node->symbol, il+2);

   if (node->ty_decl != NULL)
   {
      fprintf (out, "\n%*sty_decl =", INDENT(il+1), "");
      print_ty_t (node->ty_decl, il+2);
   }

   fprintf (out, "\n%*sinit =", INDENT(il+1), "");
   print_expr_t (node->init, il+2);

   fprintf (out, "\n%*s}", INDENT(il), "");
}

/* vim: set tags=../tags: */
