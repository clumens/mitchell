/* The parser - recognize the grammar for mitchell by repeatedely calling the
 * tokenizer to extract tokens from the input file, then feeding them into
 * our recursive descent parser.  I am purposefully avoiding using a parser
 * generator like yacc because I don't want to have to rely on things I
 * won't have when it comes time to bootstrap.
 *
 * I don't know of any ambiguities in this grammar, though I have not
 * gone through and thoroughly checked.  The grammar itself may be seen
 * in mitchell/docs/grammar, though that file is not really any more
 * descriptive than this one.
 *
 * $Id: parse.c,v 1.6 2004/10/17 23:27:40 chris Exp $
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
#include <wchar.h>

#include "tokens.h"

static FILE *in;           /* the input file */
static token_t *tok;       /* lookahead token - not yet examined */

#define NRULES   27
#define SET_SIZE 13

static const int FIRST_SET[NRULES][SET_SIZE] = {
   /* 0: branch-expr */ { BOOLEAN, IDENTIFIER, INTEGER, STRING, -1 },
   /* 1: branch-lst */ { BOOLEAN, IDENTIFIER, INTEGER, STRING, -1 },
   /* 2: case-expr */ { CASE, -1 },
   /* 3: const-decl */ { CONST, -1 },
   /* 4: const-decl-proto */ { CONST, -1 },
   /* 5: decl */ { CONST, FUNCTION, TYPE, VAR, -1 },
   /* 6: decl-expr */ { DECL, -1 },
   /* 7: decl-lst */ { CONST, FUNCTION, TYPE, VAR, -1 },
   /* 8: expr */ { BOOLEAN, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE, LBRACK,
                   STRING, -1, },
   /* 9: expr-lst */ { BOOLEAN, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE,
                       LBRACK, STRING, -1 },
   /* 10: fun-call-or-id */ { IDENTIFIER, -1 },
   /* 11: fun-decl */ { FUNCTION, -1 },
   /* 12: fun-decl-proto */ { FUNCTION, -1 },
   /* 13: id */ { IDENTIFIER, -1 },
   /* 14: id-lst */ { IDENTIFIER, -1 },
   /* 15: if-expr */ { IF, -1 },
   /* 16: module-decl */ { MODULE, -1 },
   /* 17: module-decl-lst */ { MODULE, -1 },
   /* 18: proto */ { CONST, FUNCTION, TYPE, VAR, -1 },
   /* 19: proto-lst */ { CONST, FUNCTION, TYPE, VAR, -1 },
   /* 20: record-assn-lst */ { IDENTIFIER, -1 },
   /* 21: single-ty */ { IDENTIFIER, LBRACE, -1 },
   /* 22: ty */ { IDENTIFIER, LBRACE, -1 },
   /* 23: ty-decl */ { TYPE, -1 },
   /* 24: ty-decl-proto */ { TYPE, -1 },
   /* 25: var-decl */ { VAR, -1 },
   /* 26: var-decl-proto */ { VAR, -1 }
};

static const int FOLLOW_SET[NRULES][SET_SIZE] = {
   /* 0: branch-expr */ { MAPSTO, -1 },
   /* 1: branch-lst */ { END, -1 },
   /* 2: case-expr */ { COMMA, CONST, ELSE, END, FUNCTION, IN, THEN, RBRACE,
                        RBRACK, RPAREN, TYPE, VAR, -1 },
   /* 3: const-decl */ { CONST, END, FUNCTION, IN, TYPE, VAR, -1 },
   /* 4: const-decl-proto */ { ASSIGN, CONST, FUNCTION, IN, TYPE, VAR, -1 },
   /* 5: decl */ { CONST, END, FUNCTION, IN, TYPE, VAR, -1 },
   /* 6: decl-expr */ { COMMA, CONST, ELSE, END, FUNCTION, IN, RBRACK, RBRACE,
                        RPAREN, THEN, TYPE, VAR, -1 },
   /* 7: decl-lst */ { END, IN, -1 },
   /* 8: expr */ { COMMA, CONST, ELSE, END, FUNCTION, IN, RBRACK, RBRACE,
                   RPAREN, THEN, TYPE, VAR, -1 },
   /* 9: expr-lst */ { RBRACK, RPAREN, -1 },
   /* 10: fun-call-or-id */ { COMMA, CONST, ELSE, END, FUNCTION, IN, RBRACK,
                              RBRACE, RPAREN, THEN, TYPE, VAR, -1 },
   /* 11: fun-decl */ { CONST, END, FUNCTION, IN, TYPE, VAR, -1 },
   /* 12: fun-decl-proto */ { ASSIGN, CONST, FUNCTION, IN, TYPE, VAR, -1 },
   /* 13: id */ { ASSIGN, CONST, COMMA, END, FUNCTION, IN, LIST, LPAREN,
                  RBRACE, RPAREN, TYPE, VAR, -1 },
   /* 14: id-lst */ { RBRACE, RPAREN, -1 },
   /* 15: if-expr */ { COMMA, CONST, ELSE, END, FUNCTION, IN, RBRACK, RBRACE,
                       RPAREN, THEN, TYPE, VAR, -1 },
   /* 16: module-decl */ { ENDOFFILE, MODULE, -1 },
   /* 17: module-decl-lst */ { ENDOFFILE, -1 },
   /* 18: proto */ { CONST, FUNCTION, IN, TYPE, VAR, -1 },
   /* 19: proto-lst */ { IN, -1 },
   /* 20: record-assn-lst */ { RBRACE, -1 },
   /* 21: single-ty */ { ASSIGN, CONST, COMMA, END, FUNCTION, IN, LIST, LPAREN,
                         RBRACE, RPAREN, TYPE, VAR, -1 },
   /* 22: ty */ { ASSIGN, CONST, COMMA, END, FUNCTION, IN, LPAREN, RBRACE,
                  RPAREN, TYPE, VAR, -1 },
   /* 23: ty-decl */ { CONST, END, FUNCTION, IN, TYPE, VAR, -1 },
   /* 24: ty-decl-proto */ { ASSIGN, CONST, FUNCTION, IN, TYPE, VAR, -1 },
   /* 25: var-decl */ { CONST, END, FUNCTION, IN, TYPE, VAR, -1 },
   /* 26: var-decl-proto */ { ASSIGN, CONST, FUNCTION, IN, TYPE, VAR, -1 },
};

/* These functions are seriously mutually recursive, so put forward
 * declarations of them here.
 */
static void parse_branch_expr();
static void parse_branch_lst();
static void parse_case_expr();
static void parse_const_decl();
static void parse_const_decl_proto();
static void parse_decl();
static void parse_decl_expr();
static void parse_decl_lst();
static void parse_expr();
static void parse_expr_lst();
static void parse_fun_call_or_id();
static void parse_fun_decl();
static void parse_fun_decl_proto();
static void parse_id();
static void parse_id_lst();
static void parse_if_expr();
static void parse_module_decl();
static void parse_module_decl_lst();
static void parse_proto();
static void parse_proto_lst();
static void parse_record_assn_lst();
static void parse_single_ty();
static void parse_ty();
static void parse_ty_decl();
static void parse_ty_decl_proto();
static void parse_var_decl();
static void parse_var_decl_proto();

/* Is the token t in the set?  This is used to determine membership in both
 * first and follow sets of rules by passing the appropriate rule's set from
 * one of the FIRST_SET and FOLLOW_SET tables.
 */
static unsigned int in_set (const token_t *t, const int set[])
{
   unsigned int i;

   if (t == NULL)
      return 0;

   for (i = 0; set[i] != -1; i++)
   {
      if (set[i] == t->type)
         return 1;
   }

   return 0;
}

/* Describe a token more completely so the user has a better idea of what
 * the parse error is talking about.  An even better idea would be to print
 * out a bunch of the context around the error, but I'm not feeling that
 * fancy yet.
 */
static void describe_token (const token_t *t)
{
   if (t == NULL)
      return;

   fprintf (stderr, "%s", token_map[t->type]);

   switch (t->type) {
      case BOOLEAN:
         fprintf (stderr, "(%s)", t->boolean == 0 ? "f" : "t");
         break;

      case IDENTIFIER:
         fprintf (stderr, "(%ls)", t->string);
         break;

      case INTEGER:
         fprintf (stderr, "(%li)", t->integer);
         break;

      case STRING:
         fprintf (stderr, "(%ls)", t->string);
         break;
   }
}

/* Print a parse error message, describing the token set we were expecting
 * and what we actually got.  Elaborate on what we got a little bit if
 * it was something special to give the user a little more context, too.
 */
static void parse_error(const token_t *t, const char *accepted)
{
   fprintf (stderr, "*** parse error on line %d:\n", t->lineno);
   fprintf (stderr, "*** expected token from set {%s}, but got ", accepted);
   describe_token(t);
   fprintf (stderr, " instead\n");
   exit(1);
}

/* Check the type of the lookahead token.  If we match, read the next
 * token so we'll be ready to check again in the future.  If we don't
 * match, that's a parse error and we fail.  No clever error correction
 * here.
 */
static void match (const unsigned int type)
{
   if (tok == NULL)
   {
      fprintf (stderr, "*** parse error:  premature end of input file\n");
      exit (1);
   }

   if (tok->type == type)
   {
      printf ("  ate %s\n", token_map[type]);
      
      if ((tok = next_token (in)) == NULL)
      {
         fprintf (stderr, "*** parse error:  premature end of input file\n");
         exit (1);
      }
   }
   else
      parse_error (tok, token_map[type]);
}

/* Main parser entry point - open the file, read the first token, and try
 * to parse the start symbol.
 */
void parse (const char *filename)
{
   if ((in = fopen (filename, "r")) == NULL)
   {
      fprintf (stderr, "could not open for reading: %s\n", filename);
      exit(1);
   }

   tok = next_token (in);

   if (in_set (tok, FIRST_SET[17]))
      parse_module_decl_lst();
   else
      parse_error (tok, "MODULE");

   match (ENDOFFILE);

   fclose (in);
}

/* branch-expr ::= id
 *               | INTEGER
 *               | STRING
 *               | BOOLEAN
 */
static void parse_branch_expr()
{
   printf ("entering %s\n", __FUNCTION__);

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         break;

      case IDENTIFIER:
         parse_id();
         break;

      case INTEGER:
         match(INTEGER);
         break;

      case STRING:
         match(STRING);
         break;

      default:
         parse_error (tok, "BOOLEAN IDENTIFIER INTEGER STRING");
   }

   if (!in_set (tok, FOLLOW_SET[0]))
      parse_error (tok, "MAPSTO");

   printf ("leaving %s\n", __FUNCTION__);
}

/* branch-lst ::= branch-expr MAPSTO expr
 *              | branch-expr MAPSTO expr COMMA branch-lst
 */
static void parse_branch_lst()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_branch_expr();
   match(MAPSTO);
   parse_expr();

   if (tok->type == COMMA)
   {
      match(COMMA);
      parse_branch_lst();
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* case-expr ::= CASE expr IN branch-lst END */
static void parse_case_expr()
{
   printf ("entering %s\n", __FUNCTION__);

   match(CASE);
   parse_expr();
   match(IN);
   parse_branch_lst();
   match(END);

   printf ("leaving %s\n", __FUNCTION__);
}

/* const-decl ::= const-decl-proto ASSIGN expr */
static void parse_const_decl()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_const_decl_proto();
   match(ASSIGN);
   parse_expr();

   printf ("leaving %s\n", __FUNCTION__);
}

/* const-decl-proto ::= CONST IDENTIFIER COLON ty */
static void parse_const_decl_proto()
{
   printf ("entering %s\n", __FUNCTION__);

   match(CONST);
   match(IDENTIFIER);
   match(COLON);
   parse_ty();

   printf ("leaving %s\n", __FUNCTION__);
}

/* decl ::= ty-decl
 *        | var-decl
 *        | const-decl
 *        | fun-decl
 */
static void parse_decl()
{
   printf ("entering %s\n", __FUNCTION__);

   switch (tok->type) {
      case CONST:
         parse_const_decl();
         break;

      case FUNCTION:
         parse_fun_decl();
         break;
         
      case TYPE:
         parse_ty_decl();
         break;

      case VAR:
         parse_var_decl();
         break;

      default:
         parse_error (tok, "CONST FUNCTION TYPE VAR");
   }

   if (!in_set (tok, FOLLOW_SET[5]))
      parse_error (tok, "CONST END FUNCTION IN TYPE VAR");

   printf ("leaving %s\n", __FUNCTION__);
}

/* decl-expr ::= DECL decl-lst IN expr END */
static void parse_decl_expr()
{
   printf ("entering %s\n", __FUNCTION__);

   match(DECL);
   parse_decl_lst();
   match(IN);
   parse_expr();
   match(END);

   printf ("leaving %s\n", __FUNCTION__);
}

/* decl-lst ::= decl decl-lst
 *            | decl
 */
static void parse_decl_lst()
{
   printf ("entering %s\n", __FUNCTION__);

   if (!in_set (tok, FIRST_SET[7]))
      parse_error (tok, "CONST FUNCTION TYPE VAR");

   parse_decl();

   if (!in_set (tok, FOLLOW_SET[7]))
      parse_decl_lst();

   printf ("leaving %s\n", __FUNCTION__);
}

/* expr ::= LBRACK expr-lst RBRACK
 *        | LBRACE record-assn-lst RBRACE
 *        | case-expr
 *        | decl-expr
 *        | if-expr
 *        | fun-call-or-id
 *        | INTEGER
 *        | STRING
 *        | BOOLEAN
 */
static void parse_expr()
{
   printf ("entering %s\n", __FUNCTION__);

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         break;

      case CASE:
         parse_case_expr();
         break;

      case DECL:
         parse_decl_expr();
         break;

      case IDENTIFIER:
         parse_fun_call_or_id();
         break;

      case IF:
         parse_if_expr();
         break;

      case INTEGER:
         match(INTEGER);
         break;

      case LBRACE:
         match(LBRACE);
         parse_record_assn_lst();
         match(RBRACE);
         break;

      case LBRACK:
         match(LBRACK);
         parse_expr_lst();
         match(RBRACK);
         break;

      case STRING:
         match(STRING);
         break;

      default:
         parse_error (tok, "BOOLEAN CASE DECL IDENTIFIER IF INTEGER LBRACE "
                           "LBRACK STRING");
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* expr-lst ::= expr COMMA expr-lst
 *            | expr
 */
static void parse_expr_lst()
{
   printf ("entering %s\n", __FUNCTION__);
   
   parse_expr();

   if (tok->type == COMMA)
   {
      match(COMMA);
      parse_expr_lst();
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* fun-call-or-id ::= id LPAREN expr-lst RPAREN
 *                  | id LPAREN RPAREN
 *                  | id
 */
static void parse_fun_call_or_id()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_id();

   if (tok->type == LPAREN)
   {
      match(LPAREN);

      if (tok->type != RPAREN)
      {
         parse_expr_lst();
         match(RPAREN);
      }
      else
         match(RPAREN);
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* fun-decl ::= fun-decl-proto ASSIGN expr */
static void parse_fun_decl()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_fun_decl_proto();
   match(ASSIGN);
   parse_expr();

   printf ("leaving %s\n", __FUNCTION__);
}

/* fun-decl-proto ::= FUNCTION IDENTIFIER COLON ty LPAREN id-lst RPAREN
 *                  | FUNCTION IDENTIFIER COLON ty LPAREN RPAREN
 */
static void parse_fun_decl_proto()
{
   printf ("entering %s\n", __FUNCTION__);

   match(FUNCTION);
   match(IDENTIFIER);
   match(COLON);
   parse_ty();
   match(LPAREN);

   if (tok->type == RPAREN)
      match(RPAREN);
   else
   {
      parse_id_lst();
      match(RPAREN);
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* id ::= IDENTIFIER
 *      | IDENTIFIER DOT id
 */
static void parse_id()
{
   printf ("entering %s\n", __FUNCTION__);

   match(IDENTIFIER);

   if (tok->type == DOT)
   {
      match(DOT);
      parse_id();
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* id-lst ::= IDENTIFIER COLON ty
 *          | IDENTIFIER COLON ty COMMA id-lst
 */
static void parse_id_lst()
{
   printf ("entering %s\n", __FUNCTION__);

   match(IDENTIFIER);
   match(COLON);
   parse_ty();

   if (tok->type == COMMA)
   {
      match(COMMA);
      parse_id_lst();
   }


   printf ("leaving %s\n", __FUNCTION__);
}

/* if-expr ::= IF expr THEN expr ELSE expr */
static void parse_if_expr()
{
   printf ("entering %s\n", __FUNCTION__);

   match(IF);
   parse_expr();
   match(THEN);
   parse_expr();
   match(ELSE);
   parse_expr();

   printf ("leaving %s\n", __FUNCTION__);
}

/* module-decl ::= MODULE IDENTIFIER ASSIGN DECL proto-lst IN decl-lst END */
static void parse_module_decl()
{
   printf ("entering %s\n", __FUNCTION__);

   match (MODULE);
   match (IDENTIFIER);
   match (ASSIGN);
   match (DECL);
   parse_proto_lst();
   match (IN);
   parse_decl_lst();
   match (END);

   if (!in_set (tok, FOLLOW_SET[16]))
      parse_error (tok, "ENDOFFILE MODULE");

   printf ("leaving %s\n", __FUNCTION__);
}

/* module-decl-lst ::= module-decl
 *                   | module-decl module-decl-lst
 */
static void parse_module_decl_lst()
{
   printf ("entering %s\n", __FUNCTION__);

   if (in_set (tok, FIRST_SET[16]))
   {
      parse_module_decl();
      parse_module_decl_lst();
   }
   else
   {
      if (!in_set (tok, FOLLOW_SET[17]))
         parse_error (tok, "MODULE");
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* proto ::= ty-decl-proto
 *         | var-decl-proto
 *         | const-decl-proto
 *         | fun-decl-proto
 */
static void parse_proto()
{
   printf ("entering %s\n", __FUNCTION__);

   switch (tok->type) {
      case CONST:
         parse_const_decl_proto();
         break;

      case FUNCTION:
         parse_fun_decl_proto();
         break;

      case TYPE:
         parse_ty_decl_proto();
         break;

      case VAR:
         parse_var_decl_proto();
         break;

      default:
         parse_error (tok, "CONST FUNCTION TYPE VAR");
   }

   if (!in_set (tok, FOLLOW_SET[18]))
      parse_error (tok, "CONST FUNCTION IN TYPE VAR");

   printf ("leaving %s\n", __FUNCTION__);
}

/* proto-lst ::= proto
 *             | proto proto-lst
 */
static void parse_proto_lst()
{
   printf ("entering %s\n", __FUNCTION__);

   if (!in_set (tok, FIRST_SET[19]))
      parse_error (tok, "CONST FUNCTION TYPE VAR");

   parse_proto();

   if (!in_set (tok, FOLLOW_SET[19]))
      parse_proto_lst();

   printf ("leaving %s\n", __FUNCTION__);
}

/* record-assn-lst ::= IDENTIFIER ASSIGN expr
 *                   | IDENTIFIER ASSIGN expr COMMA record-assn-lst
 */
static void parse_record_assn_lst()
{
   printf ("entering %s\n", __FUNCTION__);

   match(IDENTIFIER);
   match(ASSIGN);
   parse_expr();

   if (!in_set (tok, FOLLOW_SET[20]))
   {
      match(COMMA);
      parse_record_assn_lst();
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* single-ty ::= LBRACE id-lst RBRACE
 *             | id
 */
static void parse_single_ty()
{
   printf ("entering %s\n", __FUNCTION__);

   switch (tok->type) {
      case IDENTIFIER:
         parse_id();
         break;

      case LBRACE:
         match(LBRACE);
         parse_id_lst();
         match(RBRACE);
         break;

      default:
         parse_error (tok, "IDENTIFIER LBRACE");
   }

   printf ("leaving %s\n", __FUNCTION__);
}

/* ty ::= single-ty
 *      | single-ty LIST
 */
static void parse_ty()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_single_ty();
   
   if (tok->type == LIST)
      match(LIST);

   printf ("leaving %s\n", __FUNCTION__);
}

/* ty-decl ::= ty-decl-proto ASSIGN ty */
static void parse_ty_decl()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_ty_decl_proto();
   match(ASSIGN);
   parse_ty();

   printf ("leaving %s\n", __FUNCTION__);
}

/* ty-decl-proto ::= TYPE id */
static void parse_ty_decl_proto()
{
   printf ("entering %s\n", __FUNCTION__);

   match(TYPE);
   parse_id();

   printf ("leaving %s\n", __FUNCTION__);
}

/* var-decl ::= var-decl-proto ASSIGN expr */
static void parse_var_decl()
{
   printf ("entering %s\n", __FUNCTION__);

   parse_var_decl_proto();
   match(ASSIGN);
   parse_expr();

   printf ("leaving %s\n", __FUNCTION__);
}

/* var-decl-proto ::= VAR IDENTIFIER COLON ty */
static void parse_var_decl_proto()
{
   printf ("entering %s\n", __FUNCTION__);

   match(VAR);
   match(IDENTIFIER);
   match(COLON);
   parse_ty();

   printf ("leaving %s\n", __FUNCTION__);
}

/* vim: set tags=../tags: */
