/* This file defines the structures that make up the abstract syntax tree.
 * Finally, we get to begin the process of converting code into trees, and
 * that into lots more trees.
 *
 * $Id: absyn.h,v 1.1 2004/10/22 15:18:56 chris Exp $
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

typedef enum { AST_BOOLEAN, AST_CASE, AST_DECL, AST_EXPR_LST, AST_FUN_CALL,
               AST_ID, AST_IF, AST_INTEGER, AST_RECORD_LST,
               AST_STRING } expr_type;

typedef struct {
   /* nothing for now */
} absyn_decl_t;

typedef struct {
   char *identifier;                   /* XXX: temporary */
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
         char *identifier;             /* XXX: temporary */
         struct absyn_expr_lst_t *arg_lst;
      } fun_call_expr;

      char *identifier;                /* XXX: temporary */
      unsigned int boolean_expr;
      long int integer_expr;
      char *string_expr;
   };
} absyn_expr_t;

/* A list of expressions - used for function call arguments and record
 * assignments.
 */
typedef struct {
   absyn_expr_t *expr;
   struct expr_lst_t *next;
} absyn_expr_lst_t;

/* A list of declarations - used in decl-in-end constructs. */
typedef struct {
   absyn_decl_t *decl;
   struct decl_lst_t *next;
} absyn_decl_lst_t;

#endif

/* vim: set tags=../tags: */
