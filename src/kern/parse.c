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
 * $Id: parse.c,v 1.29 2004/12/14 02:01:00 chris Exp $
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
#include "error.h"
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

#define NRULES   21        /* number of parser rules in each set */
#define SET_SIZE 14        /* max number of elements in each rule */

enum { SET_BRANCH_EXPR, SET_BRANCH_LST, SET_CASE_EXPR, SET_DECL,
       SET_DECL_EXPR, SET_DECL_LST, SET_EXPR, SET_EXPR_LST,
       SET_FUN_CALL_OR_ID, SET_FUN_DECL, SET_ID, SET_ID_LST, SET_IF_EXPR,
       SET_MODULE_DECL, SET_MODULE_DECL_LST, SET_RECORD_ASSN_LST, SET_TOP_DECL,
       SET_TOP_DECL_LST, SET_TY, SET_TY_DECL, SET_VAL_DECL };

static const int FIRST_SET[NRULES][SET_SIZE] = {
   /* branch-expr */ { BOOLEAN, IDENTIFIER, INTEGER, STRING, -1 },
   /* branch-lst */ { BOOLEAN, ELSE, IDENTIFIER, INTEGER, STRING, -1 },
   /* case-expr */ { CASE, -1 },
   /* decl */ { FUNCTION, TYPE, VAL, -1 },
   /* decl-expr */ { DECL, -1 },
   /* decl-lst */ { FUNCTION, TYPE, VAL, -1 },
   /* expr */ { BOOLEAN, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE, LBRACK,
                STRING, -1 },
   /* expr-lst */ { BOOLEAN, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE,
                    LBRACK, STRING, -1 },
   /* fun-call-or-id */ { IDENTIFIER, -1 },
   /* fun-decl */ { FUNCTION, -1 },
   /* id */ { IDENTIFIER, -1 },
   /* id-lst */ { IDENTIFIER, -1 },
   /* if-expr */ { IF, -1 },
   /* module-decl */ { MODULE, -1 },
   /* module-decl-lst */ { MODULE, -1 },
   /* record-assn-lst */ { IDENTIFIER, -1 },
   /* top-decl */ { FUNCTION, MODULE, TYPE, VAL, -1 },
   /* top-decl-lst */ { FUNCTION, MODULE, TYPE, VAL, -1 },
   /* ty */ { IDENTIFIER, LBRACE, LIST, -1 },
   /* ty-decl */ { TYPE, -1 },
   /* val-decl */ { VAL, -1 }
};

static const int FOLLOW_SET[NRULES][SET_SIZE] = {
   /* branch-expr */ { MAPSTO, -1 },
   /* branch-lst */ { END, -1 },
   /* case-expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN,
                     THEN, TYPE, VAL, -1 },
   /* decl */ { END, FUNCTION, IN, MODULE, TYPE, VAL, -1 },
   /* decl-expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN,
                     THEN, TYPE, VAL, -1 },
   /* decl-lst */ { END, IN, -1 },
   /* expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN, THEN,
                TYPE, VAL, -1 },
   /* expr-lst */ { RBRACK, RPAREN, -1 },
   /* fun-call-or-id */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK,
                          RPAREN, THEN, TYPE, VAL, -1 },
   /* fun-decl */ { END, FUNCTION, IN, MODULE, TYPE, VAL, -1 },
   /* id */ { COMMA, ELSE, END, FUNCTION, IN, LPAREN, MAPSTO, RBRACE, RBRACK,
              RPAREN, THEN, TYPE, VAL, -1 },
   /* id-lst */ { RBRACE, RPAREN, -1 },
   /* if-expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN, THEN,
                   TYPE, VAL, -1 },
   /* module-decl */ { END, ENDOFFILE, FUNCTION, MODULE, TYPE, VAL, -1 },
   /* module-decl-lst */ { ENDOFFILE, -1 },
   /* record-assn-lst */ { RBRACE, -1 },
   /* top-decl */ { END, FUNCTION, MODULE, TYPE, VAL, -1 },
   /* top-decl-lst */ { END, -1 },
   /* ty */ { ASSIGN, COMMA, END, FUNCTION, IN, LPAREN, RBRACE, RPAREN, TYPE,
              VAL, -1 },
   /* ty-decl */ { END, FUNCTION, IN, MODULE, TYPE, VAL, -1 },
   /* val-decl */ { END, FUNCTION, IN, MODULE, TYPE, VAL, -1 }
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
static absyn_id_expr_t *parse_id();
static absyn_id_lst_t *parse_id_lst();
static absyn_if_expr_t *parse_if_expr();
static absyn_module_decl_t *parse_module_decl();
static absyn_module_lst_t *parse_module_decl_lst();
static absyn_record_lst_t *parse_record_assn_lst();
static absyn_decl_t *parse_top_decl();
static absyn_decl_lst_t *parse_top_decl_lst();
static absyn_ty_t *parse_ty();
static absyn_ty_decl_t *parse_ty_decl();
static absyn_val_decl_t *parse_val_decl();

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

/* Print out the members of the set argument. */
static void print_set (const int set[])
{
   unsigned int i;

   fprintf (stderr, "{");

   for (i = 0; set[i] != -1; i++)
      fprintf (stderr, " %s", token_map[set[i]]);

   fprintf (stderr, " }");
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
static void parse_error(const token_t *t, const int accepted[])
{
   PARSE_ERROR (compiler_config.filename, t->lineno);
   fprintf (stderr, "\texpected token from set ");
   print_set (accepted);
   fprintf (stderr, ", but got { ");
   describe_token(t);
   fprintf (stderr, " } instead\n");
   exit(1);
}

/* Check the type of the lookahead token.  If we match, read the next
 * token so we'll be ready to check again in the future.  If we don't
 * match, that's a parse error and we fail.  No clever error correction
 * here.
 */
static void match (const unsigned int type)
{
   int temp_set[2] = { type, -1 };

   if (tok == NULL)
   {
      PARSE_ERROR (compiler_config.filename, 0);
      fprintf (stderr, "\tpremature end of input file\n");
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
         PARSE_ERROR (compiler_config.filename, 0);
         fprintf (stderr, "\tpremature end of input file\n");
         exit (1);
      }
   }
   else
      parse_error (tok, temp_set);
}

/* Main parser entry point - open the file, read the first token, and try
 * to parse the start symbol.  Returns the abstract syntax tree, which is
 * built as a side-effect of the parsing.
 */
ast_t *parse (const char *filename)
{
   absyn_module_lst_t *retval = NULL;

   if ((in = fopen (filename, "r")) == NULL)
   {
      COULD_NOT_OPEN_ERROR (filename, "reading");
      exit(1);
   }

   tok = next_token (in);

   if (in_set (tok, FIRST_SET[SET_MODULE_DECL_LST]))
      retval = parse_module_decl_lst();
   else
      parse_error (tok, FIRST_SET[SET_MODULE_DECL_LST]);

   match (ENDOFFILE);

   fclose (in);
   return retval;
}

/* +=================================================================+
 * | PARSER FUNCTIONS - ONE PER RULE                                 |
 * +=================================================================+
 */

/* branch-expr ::= id
 *               | INTEGER
 *               | STRING
 *               | BOOLEAN
 */
static absyn_expr_t *parse_branch_expr()
{
   absyn_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_expr_t));

   retval->lineno = tok->lineno;

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         retval->kind = ABSYN_BOOLEAN;
         retval->boolean_expr = last_tok->boolean;
         break;

      case IDENTIFIER:
         retval->kind = ABSYN_ID;
         retval->identifier = parse_id();
         break;

      case INTEGER:
         match(INTEGER);
         retval->kind = ABSYN_INTEGER;
         retval->integer_expr = last_tok->integer;
         break;

      case STRING:
         match(STRING);
         retval->kind = ABSYN_STRING;
         retval->string_expr = last_tok->string;
         break;

      default:
         parse_error (tok, FIRST_SET[SET_BRANCH_EXPR]);
   }

   if (!in_set (tok, FOLLOW_SET[SET_BRANCH_EXPR]))
      parse_error (tok, FOLLOW_SET[SET_BRANCH_EXPR]);

   LEAVING(__FUNCTION__);
   return retval;
}

/* branch-lst ::= ELSE MAPSTO expr
 *              | branch-expr MAPSTO expr
 *              | branch-expr MAPSTO expr COMMA branch-lst
 */
static absyn_branch_lst_t *parse_branch_lst()
{
   absyn_branch_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_branch_lst_t));

   if (tok->type == ELSE)
   {
      match(ELSE);
      retval->lineno = last_tok->lineno;
      match(MAPSTO);
      retval->branch = NULL;
      retval->expr = (struct absyn_expr_t *) parse_expr();
      retval->next = NULL;
   }
   else
   {
      retval->branch = (struct absyn_expr_t *) parse_branch_expr();
      retval->lineno = retval->branch->lineno;
      match(MAPSTO);
      retval->expr = (struct absyn_expr_t *) parse_expr();

      if (tok->type == COMMA)
      {
         match(COMMA);
         retval->next = (struct branch_lst *) parse_branch_lst();
      }
      else
         retval->next = NULL;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* case-expr ::= CASE expr IN branch-lst END */
static absyn_case_expr_t *parse_case_expr()
{
   absyn_case_expr_t *retval;
   absyn_branch_lst_t *tmp, *prev_tmp;

   ENTERING (__FUNCTION__);
   MALLOC(retval, sizeof(absyn_case_expr_t));

   match(CASE);
   retval->lineno = last_tok->lineno;
   retval->test = (struct absyn_expr_t *) parse_expr();
   match(IN);

   /* Gatber up all the branches into a list.  If there is a default one, it
    * should be at the very end (as enforced by the grammar above).
    */
   retval->branch_lst = parse_branch_lst();
   match(END);

   tmp = retval->branch_lst;
   prev_tmp = NULL;

   while (tmp != NULL)
   {
      /* No branch?  This must be it. */
      if (tmp->branch == NULL)
      {
         retval->default_expr = tmp->expr;

         /* Singleton list? */
         if (prev_tmp != NULL)
            prev_tmp->next = tmp->next;
         else
            retval->branch_lst = NULL;

         break;
      }

      prev_tmp = tmp;
      tmp = tmp->next;
   }

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
   MALLOC (retval, sizeof(absyn_decl_t));

   retval->lineno = tok->lineno;

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
         parse_error (tok, FIRST_SET[SET_DECL]);
   }

   if (!in_set (tok, FOLLOW_SET[SET_DECL]))
      parse_error (tok, FOLLOW_SET[SET_DECL]);

   LEAVING(__FUNCTION__);
   return retval;
}

/* decl-expr ::= DECL decl-lst IN expr END */
static absyn_decl_expr_t *parse_decl_expr()
{
   absyn_decl_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_decl_expr_t));

   match(DECL);
   retval->lineno = last_tok->lineno;
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
   MALLOC (retval, sizeof(absyn_decl_lst_t));

   if (!in_set (tok, FIRST_SET[SET_DECL_LST]))
      parse_error (tok, FIRST_SET[SET_DECL_LST]);

   retval->decl = parse_decl();
   retval->lineno = retval->decl->lineno;

   if (!in_set (tok, FOLLOW_SET[SET_DECL_LST]))
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
   MALLOC (retval, sizeof(absyn_expr_t));

   retval->lineno = tok->lineno;

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         retval->kind = ABSYN_BOOLEAN;
         retval->boolean_expr = last_tok->boolean;
         break;

      case CASE:
         retval->kind = ABSYN_CASE;
         retval->case_expr = parse_case_expr();
         break;

      case DECL:
         retval->kind = ABSYN_DECL;
         retval->decl_expr = parse_decl_expr();
         break;

      case IDENTIFIER:
         retval = parse_fun_call_or_id();
         break;

      case IF:
         retval->kind = ABSYN_IF;
         retval->if_expr = parse_if_expr();
         break;

      case INTEGER:
         match(INTEGER);
         retval->kind = ABSYN_INTEGER;
         retval->integer_expr = last_tok->integer;
         break;

      case LBRACE:
         match(LBRACE);
         retval->kind = ABSYN_RECORD_LST;
         retval->record_assn_lst = parse_record_assn_lst();
         match(RBRACE);
         break;

      case LBRACK:
         match(LBRACK);
         retval->kind = ABSYN_EXPR_LST;
         retval->expr_lst = (struct absyn_expr_lst_t *) parse_expr_lst();
         match(RBRACK);
         break;

      case STRING:
         match(STRING);
         retval->kind = ABSYN_STRING;
         retval->string_expr = last_tok->string;
         break;

      default:
         parse_error (tok, FIRST_SET[SET_EXPR]);
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
   MALLOC (retval, sizeof(absyn_expr_lst_t));
   
   retval->expr = parse_expr();
   retval->lineno = retval->expr->lineno;

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
   MALLOC (retval, sizeof(absyn_expr_t));

   tmp = parse_id();

   retval->lineno = tmp->lineno;

   if (tok->type == LPAREN)
   {
      retval->kind = ABSYN_FUN_CALL;
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
      retval->kind = ABSYN_ID;
      retval->identifier = tmp;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* fun-decl ::= FUNCTION IDENTIFIER COLON ty LPAREN id-lst RPAREN ASSIGN expr
 *            | FUNCTION IDENTIFIER COLON ty LPAREN RPAREN ASSIGN expr
 */
static absyn_fun_decl_t *parse_fun_decl()
{
   absyn_fun_decl_t *retval;
   absyn_id_expr_t *sym;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_fun_decl_t));
   MALLOC (sym, sizeof(absyn_id_expr_t));

   match(FUNCTION);
   retval->lineno = last_tok->lineno;

   match(IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;

   retval->symbol = sym;

   match(COLON);
   retval->retval = parse_ty();

   match(LPAREN);

   if (tok->type == RPAREN)
   {
      match(RPAREN);
      retval->formals = NULL;
   }
   else
   {
      retval->formals = parse_id_lst();
      match(RPAREN);
   }

   match(ASSIGN);
   retval->body = parse_expr();

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
   MALLOC (retval, sizeof(absyn_id_expr_t));

   match(IDENTIFIER);
   retval->lineno = last_tok->lineno;
   retval->symbol = last_tok->string;

   if (tok->type == DOT)
   {
      match(DOT);
      retval->sub = (struct absyn_id_expr_t *) parse_id();
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
   absyn_id_expr_t *sym;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_id_lst_t));
   MALLOC (sym, sizeof(absyn_id_expr_t));

   match(IDENTIFIER);
   retval->lineno = last_tok->lineno;

   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;
   retval->symbol = sym;

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
   MALLOC(retval, sizeof(absyn_if_expr_t));

   match(IF);
   retval->lineno = last_tok->lineno;
   retval->test_expr = (struct absyn_expr_t *) parse_expr();
   match(THEN);
   retval->then_expr = (struct absyn_expr_t *) parse_expr();
   match(ELSE);
   retval->else_expr = (struct absyn_expr_t *) parse_expr();

   LEAVING(__FUNCTION__);
   return retval;
}

/* module-decl ::= MODULE IDENTIFIER ASSIGN DECL top-decl-lst END */
static absyn_module_decl_t *parse_module_decl()
{
   absyn_module_decl_t *retval;
   absyn_id_expr_t *sym;

   ENTERING(__FUNCTION__);
   MALLOC (retval, sizeof (absyn_module_decl_t));
   MALLOC (sym, sizeof (absyn_id_expr_t));

   match (MODULE);
   retval->lineno = last_tok->lineno;

   match (IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;
   
   retval->symbol = sym;

   match (ASSIGN);
   match (DECL);
   retval->decl_lst = (struct absyn_decl_lst_t *) parse_top_decl_lst();
   match (END);

   if (!in_set (tok, FOLLOW_SET[SET_MODULE_DECL]))
      parse_error (tok, FOLLOW_SET[SET_MODULE_DECL]);

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
   MALLOC (retval, sizeof(absyn_module_lst_t));

   if (in_set (tok, FIRST_SET[SET_MODULE_DECL]))
   {
      retval->module = parse_module_decl();
      retval->lineno = retval->module->lineno;
      retval->next = (struct absyn_module_lst_t *) parse_module_decl_lst();
   }
   else
   {
      if (!in_set (tok, FOLLOW_SET[SET_MODULE_DECL_LST]))
         parse_error (tok, FOLLOW_SET[SET_MODULE_DECL_LST]);
      else retval = NULL;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* record-assn-lst ::= IDENTIFIER ASSIGN expr
 *                   | IDENTIFIER ASSIGN expr COMMA record-assn-lst
 */
static absyn_record_lst_t *parse_record_assn_lst()
{
   absyn_record_lst_t *retval;
   absyn_id_expr_t *sym;
   
   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_record_lst_t));
   MALLOC (sym, sizeof(absyn_id_expr_t));

   match(IDENTIFIER);
   retval->lineno = last_tok->lineno;

   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;

   retval->symbol = sym;
   match(ASSIGN);
   retval->expr = (struct absyn_expr_t *) parse_expr();

   if (!in_set (tok, FOLLOW_SET[SET_RECORD_ASSN_LST]))
   {
      match(COMMA);
      retval->next = (struct absyn_record_lst_t *) parse_record_assn_lst();
   }
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* top-decl ::= decl
 *            | module-decl
 */
static absyn_decl_t *parse_top_decl()
{
   absyn_decl_t *retval = NULL;

   ENTERING (__FUNCTION__);

   switch (tok->type) {
      case MODULE:
         MALLOC (retval, sizeof (absyn_decl_t));
         retval->type = ABSYN_MODULE_DECL;
         retval->lineno = tok->lineno;
         retval->module_decl = parse_module_decl();
         break;

      case FUNCTION:
      case TYPE:
      case VAL:
         retval = parse_decl();
         retval->lineno = tok->lineno;
         break;

      default:
         parse_error (tok, FIRST_SET[SET_TOP_DECL]);
   }

   if (!in_set (tok, FOLLOW_SET[SET_TOP_DECL]))
      parse_error (tok, FOLLOW_SET[SET_TOP_DECL]);

   LEAVING(__FUNCTION__);
   return retval;
}

/* top-decl-lst ::= top-decl top-decl-lst
 *                | top-decl
 */
static absyn_decl_lst_t *parse_top_decl_lst()
{
   absyn_decl_lst_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_decl_lst_t));

   if (!in_set (tok, FIRST_SET[SET_TOP_DECL_LST]))
      parse_error (tok, FIRST_SET[SET_TOP_DECL_LST]);

   retval->decl = parse_top_decl();
   retval->lineno = retval->decl->lineno;

   if (!in_set (tok, FOLLOW_SET[SET_TOP_DECL_LST]))
      retval->next = (struct decl_lst_t *) parse_top_decl_lst();
   else
      retval->next = NULL;

   LEAVING(__FUNCTION__);
   return retval;
}

/* ty ::= LIST ty
 *      | LBRACE id-lst RBRACE
 *      | id
 */
static absyn_ty_t *parse_ty()
{
   absyn_ty_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC(retval, sizeof(absyn_ty_t));

   switch (tok->type) {
      case IDENTIFIER:
         retval->kind = ABSYN_TY_ID;
         retval->identifier = parse_id();
         retval->lineno = retval->identifier->lineno;
         break;

      case LBRACE:
         match(LBRACE);
         retval->lineno = last_tok->lineno;
         retval->kind = ABSYN_TY_RECORD;
         retval->record = parse_id_lst();
         match(RBRACE);
         break;
         
      case LIST:
         match(LIST);
         retval->lineno = last_tok->lineno;
         retval->kind = ABSYN_TY_LIST;
         retval->list = parse_ty();
         break;
         
      default:
         parse_error(tok, FIRST_SET[SET_TY]);
   }

   LEAVING (__FUNCTION__);
   return retval;
}

/* ty-decl ::= TYPE IDENTIFIER ASSIGN ty */
static absyn_ty_decl_t *parse_ty_decl()
{
   absyn_ty_decl_t *retval;
   absyn_id_expr_t *sym;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_ty_decl_t));
   MALLOC (sym, sizeof(absyn_id_expr_t));

   match(TYPE);
   retval->lineno = last_tok->lineno;

   match(IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;

   retval->symbol = sym;

   match(ASSIGN);
   retval->ty = parse_ty();

   LEAVING(__FUNCTION__);
   return retval;
}

/* val-decl ::= VAL IDENTIFIER COLON ty ASSIGN expr */
static absyn_val_decl_t *parse_val_decl()
{
   absyn_val_decl_t *retval;
   absyn_id_expr_t *sym;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_val_decl_t));
   MALLOC (sym, sizeof(absyn_id_expr_t));

   match(VAL);
   retval->lineno = last_tok->lineno;

   match(IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;

   retval->symbol = sym;

   match(COLON);
   retval->ty = parse_ty();

   match(ASSIGN);
   retval->init = parse_expr();

   LEAVING(__FUNCTION__);
   return retval;
}

/* vim: set tags=../tags: */
