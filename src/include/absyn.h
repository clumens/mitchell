/* This file defines the structures that make up the abstract syntax tree.
 * Finally, we get to begin the process of converting code into trees, and
 * that into lots more trees.
 *
 * $Id: absyn.h,v 1.29 2005/02/12 16:26:16 chris Exp $
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
#include "list.h"
#include "symtab.h"

#ifdef __cplusplus
    extern "C" {
#endif

typedef enum { LINK_BRANCH_LST, LINK_CASE_EXPR, LINK_DECL, LINK_DECL_EXPR,
               LINK_EXPR, LINK_FUN_CALL, LINK_FUN_DECL, LINK_ID_EXPR,
               LINK_ID_LST, LINK_IF_EXPR, LINK_MODULE_DECL, LINK_RECORD_ASSN,
               LINK_RECORD_REF, LINK_TY, LINK_TY_DECL,
               LINK_VAL_DECL } link_type;

/* A backlink from an AST node to its parent in the tree. */
typedef struct {
   link_type kind;
   void *ptr;
} backlink_t;

/* +================================================================+
 * | EXPRESSION AST TYPES                                           |
 * +================================================================+
 */

typedef enum { ABSYN_BOOLEAN, ABSYN_BOTTOM, ABSYN_CASE, ABSYN_DECL,
               ABSYN_EXPR_LST, ABSYN_FUN_CALL, ABSYN_ID, ABSYN_IF,
               ABSYN_INTEGER, ABSYN_RECORD_ASSN, ABSYN_RECORD_REF,
               ABSYN_STRING } expr_type;

typedef struct absyn_id_expr_t {
   unsigned int lineno, column;
   backlink_t *parent;

   mstring_t *symbol;
   struct absyn_id_expr_t *sub;
} absyn_id_expr_t;

typedef struct absyn_record_assn_t {
   unsigned int lineno, column;
   backlink_t *parent;

   absyn_id_expr_t *symbol;
   struct absyn_expr_t *expr;
} absyn_record_assn_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   struct absyn_expr_t *rec;              /* everything before the first PIPE */
   absyn_id_expr_t *element;              /* elements within that record */
} absyn_record_ref_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   list_t *decl_lst;                      /* list of absyn_decl_t */
   struct absyn_expr_t *expr;
} absyn_decl_expr_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   struct absyn_expr_t *test_expr, *then_expr, *else_expr;
} absyn_if_expr_t;

typedef struct absyn_branch_lst_t {
   unsigned int lineno, column;
   backlink_t *parent;

   struct absyn_expr_t *branch;
   struct absyn_expr_t *expr;
} absyn_branch_lst_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   struct absyn_expr_t *test;
   list_t *branch_lst;                    /* list of absyn_branch_lst_t */
   struct absyn_expr_t *default_expr;
} absyn_case_expr_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   absyn_id_expr_t *identifier;
   list_t *arg_lst;                       /* list of absyn_expr_t */
} absyn_fun_call_t;

typedef struct absyn_expr_t {
   unsigned int lineno, column;
   backlink_t *parent;

   expr_type kind;
   ty_t *ty;

   union {
      list_t *expr_lst;                   /* list of absyn_expr_t */
      list_t *record_assn_lst;            /* list of absyn_record_assn_t */
      absyn_case_expr_t *case_expr;
      absyn_decl_expr_t *decl_expr;
      absyn_if_expr_t *if_expr;
      absyn_fun_call_t *fun_call_expr;
      absyn_id_expr_t *identifier;
      absyn_record_ref_t *record_ref;
      mbool_t boolean_expr;
      mint_t integer_expr;
      mstring_t *string_expr;
   };
} absyn_expr_t;

/* +================================================================+
 * | TYPE AST TYPES                                                 |
 * +================================================================+
 */

typedef struct absyn_id_lst_t {
   unsigned int lineno, column;
   backlink_t *parent;

   absyn_id_expr_t *symbol;
   struct absyn_ty_t *ty;
} absyn_id_lst_t;

typedef struct absyn_ty_t {
   unsigned int lineno, column;
   backlink_t *parent;
   enum { ABSYN_TY_BOTTOM, ABSYN_TY_ID, ABSYN_TY_LIST, ABSYN_TY_RECORD } kind;

   union {
      absyn_id_expr_t   *identifier;
      list_t            *record;          /* list of absyn_id_lst_t */
      struct absyn_ty_t *list;
   };
} absyn_ty_t;

/* +================================================================+
 * | DECLARATION AST TYPES                                          |
 * +================================================================+
 */

typedef enum { ABSYN_FUN_DECL, ABSYN_MODULE_DECL, ABSYN_TY_DECL,
               ABSYN_VAL_DECL } decl_type;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   
   absyn_id_expr_t *symbol;
   absyn_ty_t *retval;
   list_t *formals;                       /* list of absyn_id_lst_t */
   absyn_expr_t *body;
} absyn_fun_decl_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   absyn_id_expr_t *symbol;
   absyn_ty_t *ty_decl;
} absyn_ty_decl_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   ty_t *ty;

   absyn_id_expr_t *symbol;
   absyn_ty_t *ty_decl;
   absyn_expr_t *init;
} absyn_val_decl_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;

   absyn_id_expr_t *symbol;
   list_t *decl_lst;                      /* list of absyn_decl_t */
} absyn_module_decl_t;

typedef struct {
   unsigned int lineno, column;
   backlink_t *parent;
   decl_type type;

   union {
      absyn_fun_decl_t     *fun_decl;
      absyn_module_decl_t  *module_decl;
      absyn_ty_decl_t      *ty_decl;
      absyn_val_decl_t     *val_decl;
   };
} absyn_decl_t;

/* Now after all those other subtree types, make a more fitting name for the
 * module list one so the rest of the world can just refer to it as an
 * abstract syntax tree.
 */
typedef list_t ast_t;

/* Interface to the AST printer. */
void print_absyn (ast_t *ast, compiler_config_t *config, char *header);

#ifdef __cplusplus
    }
#endif

#endif

/* vim: set tags=../tags: */
