/* This file defines the structures that make up the abstract syntax tree.
 * Finally, we get to begin the process of converting code into trees, and
 * that into lots more trees.
 *
 * $Id: absyn.h,v 1.5 2004/10/26 04:33:33 chris Exp $
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
#ifndef _ABSYN_H
#define _ABSYN_H 1

#include "basic_types.h"
#include "config.h"

/* +================================================================+
 * | EXPRESSION AST TYPES                                           |
 * +================================================================+
 */

typedef enum { ABSYN_BOOLEAN, ABSYN_CASE, ABSYN_DECL, ABSYN_EXPR_LST,
               ABSYN_FUN_CALL, ABSYN_ID, ABSYN_IF, ABSYN_INTEGER,
               ABSYN_RECORD_LST, ABSYN_STRING } expr_type;

typedef struct {
   mstring_t symbol;
   struct absyn_id_expr_t *ns;         /* "namespace" is reserved for gcc */
} absyn_id_expr_t;

typedef struct {
   absyn_id_expr_t *symbol;
   struct absyn_expr_t *expr;
   struct record_assn_lst_t *next;
} absyn_record_lst_t;

typedef struct {
   struct absyn_decl_lst_t *decl_lst;
   struct absyn_expr_t *expr;
} absyn_decl_expr_t;

typedef struct {
   struct absyn_expr_t *test_expr, *then_expr, *else_expr;
} absyn_if_expr_t;

typedef struct {
   struct absyn_expr_t *branch;
   struct absyn_expr_t *expr;
   struct branch_lst *next;
} absyn_branch_lst_t;

typedef struct {
   struct absyn_expr_t *test;
   absyn_branch_lst_t *branch_lst;
} absyn_case_expr_t;

typedef struct {
   expr_type type;

   union {
      struct absyn_expr_lst_t *expr_lst;
      absyn_record_lst_t *record_assn_lst;
      absyn_case_expr_t *case_expr;
      absyn_decl_expr_t *decl_expr;
      absyn_if_expr_t *if_expr;

      struct {
         absyn_id_expr_t *identifier;
         struct absyn_expr_lst_t *arg_lst;
      } fun_call_expr;

      absyn_id_expr_t *identifier;
      mbool_t boolean_expr;
      mint_t integer_expr;
      mstring_t string_expr;
   };
} absyn_expr_t;

/* A list of expressions - used for function call arguments and record
 * assignments.
 */
typedef struct {
   absyn_expr_t *expr;
   struct absyn_expr_lst_t *next;
} absyn_expr_lst_t;

/* +================================================================+
 * | TYPE AST TYPES                                                 |
 * +================================================================+
 */

typedef struct {
   absyn_id_expr_t *symbol;
   struct absyn_ty_t *ty;
   struct absyn_id_lst_t *next;
} absyn_id_lst_t;

typedef struct {
   unsigned int is_list;
   unsigned int is_record;

   union {
      absyn_id_expr_t *identifier;
      absyn_id_lst_t *record;
   };
} absyn_ty_t;

/* +================================================================+
 * | DECLARATION AST TYPES                                          |
 * +================================================================+
 */

typedef enum { ABSYN_FUN_DECL, ABSYN_TY_DECL, ABSYN_VAL_DECL } decl_type;

typedef struct {
   absyn_id_expr_t *symbol;
   absyn_ty_t *ty;
   absyn_id_lst_t *id_lst;
} absyn_fun_proto_t;

typedef struct {
   absyn_id_expr_t *symbol;
   absyn_ty_t *ty;
} absyn_val_proto_t;

typedef struct {
   decl_type type;

   union {
      absyn_fun_proto_t *fun_proto;
      absyn_id_expr_t *ty_proto;
      absyn_val_proto_t *val_proto;
   };
} absyn_proto_t;

typedef struct {
   absyn_proto_t *proto;
   struct absyn_proto_lst_t *next;
} absyn_proto_lst_t;

typedef struct {
   absyn_fun_proto_t *proto;
   absyn_expr_t *body;
} absyn_fun_decl_t;

typedef struct {
   absyn_id_expr_t *proto;
   absyn_ty_t *ty;
} absyn_ty_decl_t;

typedef struct {
   absyn_val_proto_t *proto;
   absyn_expr_t *init;
} absyn_val_decl_t;

typedef struct {
   absyn_id_expr_t *symbol;
   struct absyn_proto_lst_t *proto_lst;
   struct absyn_decl_lst_t *decl_lst;
} absyn_module_decl_t;

typedef struct {
   absyn_module_decl_t *module;
   struct absyn_module_lst_t *next;
} absyn_module_lst_t;

typedef struct {
   decl_type type;

   union {
      absyn_fun_decl_t *fun_decl;
      absyn_ty_decl_t *ty_decl;
      absyn_val_decl_t *val_decl;
   };
} absyn_decl_t;

/* A list of declarations - used in decl-in-end constructs. */
typedef struct {
   absyn_decl_t *decl;
   struct absyn_decl_lst_t *next;
} absyn_decl_lst_t;

/* Now after all those other subtree types, make a more fitting name for the
 * module list one so the rest of the world can just refer to it as an
 * abstract syntax tree.
 */
typedef absyn_module_lst_t ast_t;

/* Interface to the AST printer. */
void print_absyn (ast_t *ast, compiler_config_t *config);

#endif

/* vim: set tags=../tags: */
