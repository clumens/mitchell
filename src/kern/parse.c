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
 * $Id: parse.c,v 1.14 2004/10/24 01:32:44 chris Exp $
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
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

#include "absyn.h"
#include "config.h"
#include "memory.h"
#include "tokens.h"

static FILE *in;                    /* the input file */
static token_t *tok;                /* lookahead token - not yet examined */
static token_t *last_tok = NULL;    /* previous token - needed for AST */

/* Macros to control debug printing only if the appropriate level is set. */
#define ENTERING(fn) do { if (compiler_config.debug.parser_debug > 0) \
                             printf ("entering %s\n", fn); \
                        } while (0)

#define LEAVING(fn)  do { if (compiler_config.debug.parser_debug > 0) \
                             printf ("leaving %s\n", fn); \
                        } while (0)

#define NRULES   25        /* number of parser rules in each set */
#define SET_SIZE 13        /* max number of elements in each rule */

static const int FIRST_SET[NRULES][SET_SIZE] = {
   /* 0: branch-expr */ { BOOLEAN, IDENTIFIER, INTEGER, STRING, -1 },
   /* 1: branch-lst */ { BOOLEAN, IDENTIFIER, INTEGER, STRING, -1 },
   /* 2: case-expr */ { CASE, -1 },
   /* 3: decl */ { FUNCTION, TYPE, VAL, -1 },
   /* 4: decl-expr */ { DECL, -1 },
   /* 5: decl-lst */ { FUNCTION, TYPE, VAL, -1 },
   /* 6: expr */ { BOOLEAN, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE, LBRACK,
                   STRING, -1, },
   /* 7: expr-lst */ { BOOLEAN, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE,
                       LBRACK, STRING, -1 },
   /* 8: fun-call-or-id */ { IDENTIFIER, -1 },
   /* 9: fun-decl */ { FUNCTION, -1 },
   /* 10: fun-decl-proto */ { FUNCTION, -1 },
   /* 11: id */ { IDENTIFIER, -1 },
   /* 12: id-lst */ { IDENTIFIER, -1 },
   /* 13: if-expr */ { IF, -1 },
   /* 14: module-decl */ { MODULE, -1 },
   /* 15: module-decl-lst */ { MODULE, -1 },
   /* 16: proto */ { FUNCTION, TYPE, VAL, -1 },
   /* 17: proto-lst */ { FUNCTION, TYPE, VAL, -1 },
   /* 18: record-assn-lst */ { IDENTIFIER, -1 },
   /* 19: single-ty */ { IDENTIFIER, LBRACE, -1 },
   /* 20: ty */ { IDENTIFIER, LBRACE, -1 },
   /* 21: ty-decl */ { TYPE, -1 },
   /* 22: ty-decl-proto */ { TYPE, -1 },
   /* 23: val-decl */ { VAL, -1 },
   /* 24: val-decl-proto */ { VAL, -1 }
};

static const int FOLLOW_SET[NRULES][SET_SIZE] = {
   /* 0: branch-expr */ { MAPSTO, -1 },
   /* 1: branch-lst */ { END, -1 },
   /* 2: case-expr */ { COMMA, ELSE, END, FUNCTION, IN, THEN, RBRACE,
                        RBRACK, RPAREN, TYPE, VAL, -1 },
   /* 3: decl */ { END, FUNCTION, IN, TYPE, VAL, -1 },
   /* 4: decl-expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACK, RBRACE,
                        RPAREN, THEN, TYPE, VAL, -1 },
   /* 5: decl-lst */ { END, IN, -1 },
   /* 6: expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACK, RBRACE,
                   RPAREN, THEN, TYPE, VAL, -1 },
   /* 7: expr-lst */ { RBRACK, RPAREN, -1 },
   /* 8: fun-call-or-id */ { COMMA, ELSE, END, FUNCTION, IN, RBRACK,
                              RBRACE, RPAREN, THEN, TYPE, VAL, -1 },
   /* 9: fun-decl */ { END, FUNCTION, IN, TYPE, VAL, -1 },
   /* 10: fun-decl-proto */ { ASSIGN, FUNCTION, IN, TYPE, VAL, -1 },
   /* 11: id */ { ASSIGN, COMMA, END, FUNCTION, IN, LIST, LPAREN,
                  RBRACE, RPAREN, TYPE, VAL, -1 },
   /* 12: id-lst */ { RBRACE, RPAREN, -1 },
   /* 13: if-expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACK, RBRACE,
                       RPAREN, THEN, TYPE, VAL, -1 },
   /* 14: module-decl */ { ENDOFFILE, MODULE, -1 },
   /* 15: module-decl-lst */ { ENDOFFILE, -1 },
   /* 16: proto */ { FUNCTION, IN, TYPE, VAL, -1 },
   /* 17: proto-lst */ { IN, -1 },
   /* 18: record-assn-lst */ { RBRACE, -1 },
   /* 19: single-ty */ { ASSIGN, COMMA, END, FUNCTION, IN, LIST, LPAREN,
                         RBRACE, RPAREN, TYPE, VAL, -1 },
   /* 20: ty */ { ASSIGN, COMMA, END, FUNCTION, IN, LPAREN, RBRACE,
                  RPAREN, TYPE, VAL, -1 },
   /* 21: ty-decl */ { END, FUNCTION, IN, TYPE, VAL, -1 },
   /* 22: ty-decl-proto */ { ASSIGN, FUNCTION, IN, TYPE, VAL, -1 },
   /* 23: val-decl */ { END, FUNCTION, IN, TYPE, VAL, -1 },
   /* 24: val-decl-proto */ { ASSIGN, FUNCTION, IN, TYPE, VAL, -1 },
};

/* These functions are seriously mutually recursive, so put forward
 * declarations of them here.
 */
static absyn_expr_t *parse_branch_expr();
static absyn_branch_lst_t *parse_branch_lst();
static absyn_case_expr_t *parse_case_expr();
static absyn_decl_t *parse_decl();
static absyn_decl_expr_t *parse_decl_expr();
static absyn_decl_lst_t *parse_decl_lst();
static absyn_expr_t *parse_expr();
static absyn_expr_lst_t *parse_expr_lst();
static absyn_expr_t *parse_fun_call_or_id();
static absyn_fun_decl_t *parse_fun_decl();
static absyn_fun_proto_t *parse_fun_decl_proto();
static absyn_id_expr_t *parse_id();
static absyn_id_lst_t *parse_id_lst();
static absyn_if_expr_t *parse_if_expr();
static absyn_module_decl_t *parse_module_decl();
static absyn_module_lst_t *parse_module_decl_lst();
static absyn_proto_t *parse_proto();
static absyn_proto_lst_t *parse_proto_lst();
static absyn_record_lst_t *parse_record_assn_lst();
static absyn_ty_t *parse_single_ty();
static absyn_ty_t *parse_ty();
static absyn_ty_decl_t *parse_ty_decl();
static absyn_id_expr_t *parse_ty_decl_proto();
static absyn_val_decl_t *parse_val_decl();
static absyn_val_proto_t *parse_val_decl_proto();

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

   switch (t->type) {
      case BOOLEAN:
         fprintf (stderr, "%s(%s)", token_map[t->type],
                                    t->boolean == 0 ? "f" : "t");
         break;

      case IDENTIFIER:
         fprintf (stderr, "%s(%ls)", token_map[t->type], t->string);
         break;

      case INTEGER:
         fprintf (stderr, "%s(%li)", token_map[t->type], t->integer);
         break;

      case STRING:
         fprintf (stderr, "%s(%ls)", token_map[t->type], t->string);
         break;

      default:
         fprintf (stderr, "%s", token_map[t->type]);
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
      if (compiler_config.debug.parser_debug > 0)
         printf ("  ate %s\n", token_map[type]);

      /* Update pointer to previous token so we can make AST chunks. */
      last_tok = tok;
      
      /* Grab a new token out of the stream for lookahead. */
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
ast_t *parse (const char *filename)
{
   absyn_module_lst_t *retval = NULL;

   if ((in = fopen (filename, "r")) == NULL)
   {
      fprintf (stderr, "could not open for reading: %s\n", filename);
      exit(1);
   }

   tok = next_token (in);

   if (in_set (tok, FIRST_SET[15]))
      retval = parse_module_decl_lst();
   else
      parse_error (tok, "MODULE");

   match (ENDOFFILE);

   fclose (in);
   return retval;
}

/* branch-expr ::= id
 *               | INTEGER
 *               | STRING
 *               | BOOLEAN
 */
static absyn_expr_t *parse_branch_expr()
{
   absyn_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_expr_t))

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         retval->type = ABSYN_BOOLEAN;
         retval->boolean_expr = last_tok->boolean;
         break;

      case IDENTIFIER:
         retval->type = ABSYN_ID;
         retval->identifier = parse_id();
         break;

      case INTEGER:
         match(INTEGER);
         retval->type = ABSYN_INTEGER;
         retval->integer_expr = last_tok->integer;
         break;

      case STRING:
         match(STRING);
         retval->type = ABSYN_STRING;
         retval->string_expr = last_tok->string;
         break;

      default:
         parse_error (tok, "BOOLEAN IDENTIFIER INTEGER STRING");
   }

   if (!in_set (tok, FOLLOW_SET[0]))
      parse_error (tok, "MAPSTO");

   LEAVING(__FUNCTION__);
   return retval;
}

/* branch-lst ::= branch-expr MAPSTO expr
 *              | branch-expr MAPSTO expr COMMA branch-lst
 */
static absyn_branch_lst_t *parse_branch_lst()
{
   absyn_branch_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_branch_lst_t))

   retval->branch = (struct absyn_expr_t *) parse_branch_expr();
   match(MAPSTO);
   retval->expr = (struct absyn_expr_t *) parse_expr();

   if (tok->type == COMMA)
   {
      match(COMMA);
      retval->next = (struct branch_lst *) parse_branch_lst();
   }
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* case-expr ::= CASE expr IN branch-lst END */
static absyn_case_expr_t *parse_case_expr()
{
   absyn_case_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC(retval, sizeof(absyn_case_expr_t))

   match(CASE);
   retval->test = (struct absyn_expr_t *) parse_expr();
   match(IN);
   retval->branch_lst = parse_branch_lst();
   match(END);

   LEAVING(__FUNCTION__);
   return retval;
}

/* decl ::= ty-decl
 *        | val-decl
 *        | fun-decl
 */
static absyn_decl_t *parse_decl()
{
   absyn_decl_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_decl_t))

   switch (tok->type) {
      case FUNCTION:
         retval->type = ABSYN_FUN_DECL;
         retval->fun_decl = parse_fun_decl();
         break;
         
      case TYPE:
         retval->type = ABSYN_TY_DECL;
         retval->ty_decl = parse_ty_decl();
         break;

      case VAL:
         retval->type = ABSYN_VAL_DECL;
         retval->val_decl = parse_val_decl();
         break;

      default:
         parse_error (tok, "FUNCTION TYPE VAL");
   }

   if (!in_set (tok, FOLLOW_SET[3]))
      parse_error (tok, "END FUNCTION IN TYPE VAL");

   LEAVING(__FUNCTION__);
   return retval;
}

/* decl-expr ::= DECL decl-lst IN expr END */
static absyn_decl_expr_t *parse_decl_expr()
{
   absyn_decl_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_decl_expr_t))

   match(DECL);
   retval->decl_lst = (struct absyn_decl_lst_t *) parse_decl_lst();
   match(IN);
   retval->expr = (struct absyn_expr_t *) parse_expr();
   match(END);

   LEAVING(__FUNCTION__);
   return retval;
}

/* decl-lst ::= decl decl-lst
 *            | decl
 */
static absyn_decl_lst_t *parse_decl_lst()
{
   absyn_decl_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_decl_lst_t))

   if (!in_set (tok, FIRST_SET[5]))
      parse_error (tok, "FUNCTION TYPE VAL");

   retval->decl = parse_decl();

   if (!in_set (tok, FOLLOW_SET[5]))
      retval->next = (struct decl_lst_t *) parse_decl_lst();
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
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
static absyn_expr_t *parse_expr()
{
   absyn_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_expr_t))

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         retval->type = ABSYN_BOOLEAN;
         retval->boolean_expr = last_tok->boolean;
         break;

      case CASE:
         retval->type = ABSYN_CASE;
         retval->case_expr = parse_case_expr();
         break;

      case DECL:
         retval->type = ABSYN_DECL;
         retval->decl_expr = parse_decl_expr();
         break;

      case IDENTIFIER:
         retval = parse_fun_call_or_id();
         break;

      case IF:
         retval->type = ABSYN_IF;
         retval->if_expr = parse_if_expr();
         break;

      case INTEGER:
         match(INTEGER);
         retval->type = ABSYN_INTEGER;
         retval->integer_expr = last_tok->integer;
         break;

      case LBRACE:
         match(LBRACE);
         retval->type = ABSYN_RECORD_LST;
         retval->record_assn_lst = parse_record_assn_lst();
         match(RBRACE);
         break;

      case LBRACK:
         match(LBRACK);
         retval->type = ABSYN_EXPR_LST;
         retval->expr_lst = (struct absyn_expr_lst_t *) parse_expr_lst();
         match(RBRACK);
         break;

      case STRING:
         match(STRING);
         retval->type = ABSYN_STRING;
         retval->string_expr = last_tok->string;
         break;

      default:
         parse_error (tok, "BOOLEAN CASE DECL IDENTIFIER IF INTEGER LBRACE "
                           "LBRACK STRING");
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* expr-lst ::= expr COMMA expr-lst
 *            | expr
 */
static absyn_expr_lst_t *parse_expr_lst()
{
   absyn_expr_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_expr_lst_t))
   
   retval->expr = parse_expr();

   if (tok->type == COMMA)
   {
      match(COMMA);
      retval->next = (struct expr_lst_t *) parse_expr_lst();
   }
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* fun-call-or-id ::= id LPAREN expr-lst RPAREN
 *                  | id LPAREN RPAREN
 *                  | id
 */
static absyn_expr_t *parse_fun_call_or_id()
{
   absyn_expr_t *retval;
   absyn_id_expr_t *tmp;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_expr_t))

   tmp = parse_id();

   if (tok->type == LPAREN)
   {
      retval->type = ABSYN_FUN_CALL;
      retval->fun_call_expr.identifier = tmp;

      match(LPAREN);

      if (tok->type != RPAREN)
      {
         retval->fun_call_expr.arg_lst =
            (struct absyn_expr_lst_t *) parse_expr_lst();
         match(RPAREN);
      }
      else
      {
         match(RPAREN);
         retval->fun_call_expr.arg_lst = NULL;
      }
   }
   else
   {
      retval->type = ABSYN_ID;
      retval->identifier = tmp;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* fun-decl ::= fun-decl-proto ASSIGN expr */
static absyn_fun_decl_t *parse_fun_decl()
{
   absyn_fun_decl_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_fun_decl_t))

   retval->proto = parse_fun_decl_proto();
   match(ASSIGN);
   retval->body = parse_expr();

   LEAVING(__FUNCTION__);
   return retval;
}

/* fun-decl-proto ::= FUNCTION IDENTIFIER COLON ty LPAREN id-lst RPAREN
 *                  | FUNCTION IDENTIFIER COLON ty LPAREN RPAREN
 */
static absyn_fun_proto_t *parse_fun_decl_proto()
{
   absyn_fun_proto_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_fun_proto_t))

   match(FUNCTION);
   match(IDENTIFIER);
   retval->symbol = last_tok->string;

   match(COLON);
   retval->ty = parse_ty();

   match(LPAREN);

   if (tok->type == RPAREN)
   {
      match(RPAREN);
      retval->id_lst = NULL;
   }
   else
   {
      retval->id_lst = parse_id_lst();
      match(RPAREN);
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* id ::= IDENTIFIER
 *      | IDENTIFIER DOT id
 */
static absyn_id_expr_t *parse_id()
{
   absyn_id_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_id_expr_t))

   match(IDENTIFIER);
   retval->symbol = last_tok->string;

   if (tok->type == DOT)
   {
      match(DOT);
      retval->ns = (struct absyn_id_expr_t *) parse_id();
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* id-lst ::= IDENTIFIER COLON ty
 *          | IDENTIFIER COLON ty COMMA id-lst
 */
static absyn_id_lst_t *parse_id_lst()
{
   absyn_id_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_id_lst_t))

   match(IDENTIFIER);
   retval->symbol = last_tok->string;

   match(COLON);
   retval->ty = (struct absyn_ty_t *) parse_ty();

   if (tok->type == COMMA)
   {
      match(COMMA);
      retval->next = (struct absyn_id_lst_t *) parse_id_lst();
   }
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* if-expr ::= IF expr THEN expr ELSE expr */
static absyn_if_expr_t *parse_if_expr()
{
   absyn_if_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC(retval, sizeof(absyn_if_expr_t))

   match(IF);
   retval->test_expr = (struct absyn_expr_t *) parse_expr();
   match(THEN);
   retval->then_expr = (struct absyn_expr_t *) parse_expr();
   match(ELSE);
   retval->else_expr = (struct absyn_expr_t *) parse_expr();

   LEAVING(__FUNCTION__);
   return retval;
}

/* module-decl ::= MODULE IDENTIFIER ASSIGN DECL proto-lst IN decl-lst END */
static absyn_module_decl_t *parse_module_decl()
{
   absyn_module_decl_t *retval;

   ENTERING(__FUNCTION__);
   MALLOC (retval, sizeof (absyn_module_decl_t))

   match (MODULE);
   match (IDENTIFIER);
   retval->symbol = last_tok->string;

   match (ASSIGN);
   match (DECL);
   retval->proto_lst = (struct absyn_proto_lst_t *) parse_proto_lst();

   match (IN);
   retval->decl_lst = (struct absyn_decl_lst_t *) parse_decl_lst();
   match (END);

   if (!in_set (tok, FOLLOW_SET[14]))
      parse_error (tok, "ENDOFFILE MODULE");

   LEAVING(__FUNCTION__);
   return retval;
}

/* module-decl-lst ::= module-decl
 *                   | module-decl module-decl-lst
 */
static absyn_module_lst_t *parse_module_decl_lst()
{
   absyn_module_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_module_lst_t))

   if (in_set (tok, FIRST_SET[14]))
   {
      retval->module = parse_module_decl();
      retval->next = (struct absyn_module_lst_t *) parse_module_decl_lst();
   }
   else
   {
      if (!in_set (tok, FOLLOW_SET[15]))
         parse_error (tok, "MODULE");
      else retval = NULL;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* proto ::= ty-decl-proto
 *         | val-decl-proto
 *         | fun-decl-proto
 */
static absyn_proto_t *parse_proto()
{
   absyn_proto_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_proto_t))

   switch (tok->type) {
      case FUNCTION:
         retval->type = ABSYN_FUN_DECL;
         retval->fun_proto = parse_fun_decl_proto();
         break;

      case TYPE:
         retval->type = ABSYN_TY_DECL;
         retval->ty_proto = parse_ty_decl_proto();
         break;

      case VAL:
         retval->type = ABSYN_VAL_DECL;
         retval->val_proto = parse_val_decl_proto();
         break;

      default:
         parse_error (tok, "FUNCTION TYPE VAL");
   }

   if (!in_set (tok, FOLLOW_SET[16]))
      parse_error (tok, "FUNCTION IN TYPE VAL");

   LEAVING(__FUNCTION__);
   return retval;
}

/* proto-lst ::= proto
 *             | proto proto-lst
 */
static absyn_proto_lst_t *parse_proto_lst()
{
   absyn_proto_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_proto_lst_t))

   if (!in_set (tok, FIRST_SET[17]))
      parse_error (tok, "FUNCTION TYPE VAL");

   retval->proto = parse_proto();

   if (!in_set (tok, FOLLOW_SET[17]))
      retval->next = (struct absyn_proto_lst_t *) parse_proto_lst();
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* record-assn-lst ::= IDENTIFIER ASSIGN expr
 *                   | IDENTIFIER ASSIGN expr COMMA record-assn-lst
 */
static absyn_record_lst_t *parse_record_assn_lst()
{
   absyn_record_lst_t *retval;
   
   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_record_lst_t))

   match(IDENTIFIER);
   retval->symbol = last_tok->string;
   match(ASSIGN);
   retval->expr = (struct absyn_expr_t *) parse_expr();

   if (!in_set (tok, FOLLOW_SET[18]))
   {
      match(COMMA);
      retval->next = (struct absyn_record_lst_t *) parse_record_assn_lst();
   }
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* single-ty ::= LBRACE id-lst RBRACE
 *             | id
 */
static absyn_ty_t *parse_single_ty()
{
   absyn_ty_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_ty_t))

   switch (tok->type) {
      case IDENTIFIER:
         retval->is_record = 0;
         retval->identifier = parse_id();
         break;

      case LBRACE:
         match(LBRACE);
         retval->is_record = 1;
         retval->record = parse_id_lst();
         match(RBRACE);
         break;

      default:
         parse_error (tok, "IDENTIFIER LBRACE");
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* ty ::= single-ty
 *      | single-ty LIST
 */
static absyn_ty_t *parse_ty()
{
   absyn_ty_t *retval;

   ENTERING (__FUNCTION__);

   retval = parse_single_ty();
   
   if (tok->type == LIST)
   {
      match(LIST);
      retval->is_list = 1;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* ty-decl ::= ty-decl-proto ASSIGN ty */
static absyn_ty_decl_t *parse_ty_decl()
{
   absyn_ty_decl_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_ty_decl_t))

   retval->proto = parse_ty_decl_proto();
   match(ASSIGN);
   retval->ty = parse_ty();

   LEAVING(__FUNCTION__);
   return retval;
}

/* ty-decl-proto ::= TYPE id */
static absyn_id_expr_t *parse_ty_decl_proto()
{
   absyn_id_expr_t *retval;

   ENTERING (__FUNCTION__);

   match(TYPE);
   retval = parse_id();

   LEAVING(__FUNCTION__);
   return retval;
}

/* val-decl ::= val-decl-proto ASSIGN expr */
static absyn_val_decl_t *parse_val_decl()
{
   absyn_val_decl_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_val_decl_t))

   retval->proto = parse_val_decl_proto();
   match(ASSIGN);
   retval->init = parse_expr();

   LEAVING(__FUNCTION__);
   return retval;
}

/* val-decl-proto ::= VAL IDENTIFIER COLON ty */
static absyn_val_proto_t *parse_val_decl_proto()
{
   absyn_val_proto_t *retval;
   
   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_val_proto_t))

   match(VAL);
   match(IDENTIFIER);
   retval->symbol = last_tok->string;

   match(COLON);
   retval->ty = parse_ty();

   LEAVING(__FUNCTION__);
   return retval;
}

/* vim: set tags=../tags: */
