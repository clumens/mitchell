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
 * $Id: parse.c,v 1.37 2005/01/19 04:29:33 chris Exp $
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

#define NRULES   23        /* number of parser rules in each set */
#define SET_SIZE 15        /* max number of elements in each rule */

enum { SET_BRANCH_EXPR, SET_BRANCH_LST, SET_CASE_EXPR, SET_DECL,
       SET_DECL_EXPR, SET_DECL_LST, SET_EXPR, SET_EXPR_LST,
       SET_FUN_CALL_OR_ID, SET_FUN_DECL, SET_ID, SET_ID_LST, SET_IF_EXPR,
       SET_MODULE_DECL, SET_MODULE_DECL_LST, SET_RECORD_ASSN_LST,
       SET_RECORD_REF, SET_SYM_REF, SET_TOP_DECL, SET_TOP_DECL_LST, SET_TY,
       SET_TY_DECL, SET_VAL_DECL };

static const int FIRST_SET[NRULES][SET_SIZE] = {
   /* branch-expr */ { BOOLEAN, IDENTIFIER, INTEGER, STRING, -1 },
   /* branch-lst */ { BOOLEAN, ELSE, IDENTIFIER, INTEGER, STRING, -1 },
   /* case-expr */ { CASE, -1 },
   /* decl */ { FUNCTION, TYPE, VAL, -1 },
   /* decl-expr */ { DECL, -1 },
   /* decl-lst */ { FUNCTION, TYPE, VAL, -1 },
   /* expr */ { BOOLEAN, BOTTOM, CASE, DECL, IDENTIFIER, IF, INTEGER, LBRACE,
                LBRACK, STRING, -1 },
   /* expr-lst */ { BOOLEAN, BOTTOM, CASE, DECL, IDENTIFIER, IF, INTEGER,
                    LBRACE, LBRACK, STRING, -1 },
   /* fun-call-or-id */ { IDENTIFIER, -1 },
   /* fun-decl */ { FUNCTION, -1 },
   /* id */ { IDENTIFIER, -1 },
   /* id-lst */ { IDENTIFIER, -1 },
   /* if-expr */ { IF, -1 },
   /* module-decl */ { MODULE, -1 },
   /* module-decl-lst */ { MODULE, -1 },
   /* record-assn-lst */ { IDENTIFIER, -1 },
   /* record-ref */ { IDENTIFIER, -1 },
   /* sym-ref */ { IDENTIFIER, -1 },
   /* top-decl */ { FUNCTION, MODULE, TYPE, VAL, -1 },
   /* top-decl-lst */ { FUNCTION, MODULE, TYPE, VAL, -1 },
   /* ty */ { BOTTOM, IDENTIFIER, LBRACE, LIST, -1 },
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
   /* fun-call-or-id */ { COMMA, ELSE, END, FUNCTION, IN, PIPE, RBRACE, RBRACK,
                          RPAREN, THEN, TYPE, VAL, -1 },
   /* fun-decl */ { END, FUNCTION, IN, MODULE, TYPE, VAL, -1 },
   /* id */ { COMMA, ELSE, END, FUNCTION, IN, LPAREN, MAPSTO, PIPE, RBRACE,
              RBRACK, RPAREN, THEN, TYPE, VAL, -1 },
   /* id-lst */ { RBRACE, RPAREN, -1 },
   /* if-expr */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN, THEN,
                   TYPE, VAL, -1 },
   /* module-decl */ { END, ENDOFFILE, FUNCTION, MODULE, TYPE, VAL, -1 },
   /* module-decl-lst */ { ENDOFFILE, -1 },
   /* record-assn-lst */ { RBRACE, -1 },
   /* record-ref */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN,
                      THEN, TYPE, VAL, -1 },
   /* sym-ref */ { COMMA, ELSE, END, FUNCTION, IN, RBRACE, RBRACK, RPAREN, THEN,
                   TYPE, VAL, -1 },
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
static list_t *parse_branch_lst();
static absyn_case_expr_t *parse_case_expr();
static absyn_decl_t *parse_decl();
static absyn_decl_expr_t *parse_decl_expr();
static list_t *parse_decl_lst();
static absyn_expr_t *parse_expr();
static list_t *parse_expr_lst();
static absyn_expr_t *parse_fun_call_or_id();
static absyn_fun_decl_t *parse_fun_decl();
static absyn_id_expr_t *parse_id();
static list_t *parse_id_lst();
static absyn_if_expr_t *parse_if_expr();
static absyn_module_decl_t *parse_module_decl();
static list_t *parse_module_decl_lst();
static list_t *parse_record_assn_lst();
static absyn_id_expr_t *parse_record_ref();
static absyn_expr_t *parse_sym_ref();
static absyn_decl_t *parse_top_decl();
static list_t *parse_top_decl_lst();
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
   PARSE_ERROR (compiler_config.filename, t->lineno, t->column);
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
      PARSE_ERROR (compiler_config.filename, 0, 0);
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
         PARSE_ERROR (compiler_config.filename, 0, 0);
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
   list_t *retval = NULL;

   if ((in = fopen (filename, "r")) == NULL)
   {
      COULD_NOT_OPEN_ERROR (filename, "reading");
      exit(1);
   }

   tok = next_token (in);
   retval = parse_module_decl_lst();
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
   retval->column = tok->column;

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
static list_t *parse_branch_lst()
{
   list_t *retval = NULL;

   ENTERING (__FUNCTION__);

   while (1) {
      absyn_branch_lst_t *new_ele;

      if (tok->type == ELSE)
      {
         MALLOC (new_ele, sizeof(absyn_branch_lst_t));

         match(ELSE);
         new_ele->lineno = last_tok->lineno;
         new_ele->column = last_tok->column;
         match(MAPSTO);
         new_ele->branch = NULL;
         new_ele->expr = parse_expr();

         /* The else branch has to come last. */
         if (!in_set (tok, FOLLOW_SET[SET_BRANCH_LST]))
            parse_error (tok, FOLLOW_SET[SET_BRANCH_LST]);

         retval = list_append (retval, new_ele);
         LEAVING(__FUNCTION__);
         return retval;
      }
      else
      {
         MALLOC (new_ele, sizeof(absyn_branch_lst_t));

         new_ele->branch = parse_branch_expr();
         new_ele->lineno = new_ele->branch->lineno;
         new_ele->column = new_ele->branch->column;
         match(MAPSTO);
         new_ele->expr = parse_expr();

         retval = list_append (retval, new_ele);
      }

      if (in_set (tok, FOLLOW_SET[SET_BRANCH_LST]))
         break;
      else
         match(COMMA);
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* case-expr ::= CASE expr IN branch-lst END */
static absyn_case_expr_t *parse_case_expr()
{
   absyn_case_expr_t *retval;
   absyn_branch_lst_t *last_branch;

   ENTERING (__FUNCTION__);
   MALLOC(retval, sizeof(absyn_case_expr_t));

   match(CASE);
   retval->lineno = last_tok->lineno;
   retval->column = last_tok->column;
   retval->test = parse_expr();
   match(IN);

   /* Gatber up all the branches into a list.  If there is a default one, it
    * will be at the very end, as enforced by parse_branch_lst().  In this
    * case, we need to pull it out of the list and put it in the default_expr
    * slot of retval.
    */
   retval->branch_lst = parse_branch_lst();

   last_branch = (list_tl(retval->branch_lst))->data;
   if (last_branch->branch == NULL)
   {
      retval->default_expr = last_branch->expr;
      retval->branch_lst = list_remove_tl (retval->branch_lst);
   }
   
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
   MALLOC (retval, sizeof(absyn_decl_t));

   retval->lineno = tok->lineno;
   retval->column = tok->column;

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
   retval->column = last_tok->column;
   retval->decl_lst = parse_decl_lst();
   match(IN);
   retval->expr = parse_expr();
   match(END);

   LEAVING(__FUNCTION__);
   return retval;
}

/* decl-lst ::= decl decl-lst
 *            | decl
 */
static list_t *parse_decl_lst()
{
   list_t *retval = NULL;

   ENTERING (__FUNCTION__);

   while (1) {
      if (!in_set (tok, FIRST_SET[SET_DECL_LST]))
         parse_error (tok, FIRST_SET[SET_DECL_LST]);

      retval = list_append (retval, parse_decl());

      if (in_set (tok, FOLLOW_SET[SET_DECL_LST]))
         break;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* expr ::= LBRACK expr-lst RBRACK
 *        | LBRACE record-assn-lst RBRACE
 *        | case-expr
 *        | decl-expr
 *        | if-expr
 *        | sym-ref
 *        | INTEGER
 *        | STRING
 *        | BOOLEAN
 *        | BOTTOM
 */
static absyn_expr_t *parse_expr()
{
   absyn_expr_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof(absyn_expr_t));

   retval->lineno = tok->lineno;
   retval->column = tok->column;

   switch (tok->type) {
      case BOOLEAN:
         match(BOOLEAN);
         retval->kind = ABSYN_BOOLEAN;
         retval->boolean_expr = last_tok->boolean;
         break;

      case BOTTOM:
         match(BOTTOM);
         retval->kind = ABSYN_BOTTOM;
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
         retval = parse_sym_ref();
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
         retval->kind = ABSYN_RECORD_ASSN;
         retval->record_assn_lst = parse_record_assn_lst();
         match(RBRACE);
         break;

      case LBRACK:
         match(LBRACK);
         retval->kind = ABSYN_EXPR_LST;
         retval->expr_lst = parse_expr_lst();
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
static list_t *parse_expr_lst()
{
   list_t *retval = NULL;

   ENTERING (__FUNCTION__);

   while (1) {
      retval = list_append (retval, parse_expr());

      if (in_set (tok, FOLLOW_SET[SET_EXPR_LST]))
         break;
      else
         match(COMMA);
   }

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
   retval->column = tmp->column;

   if (!in_set (tok, FOLLOW_SET[SET_FUN_CALL_OR_ID]))
   {
      match(LPAREN);

      MALLOC (retval->fun_call_expr, sizeof(absyn_fun_call_t));

      retval->kind = ABSYN_FUN_CALL;
      retval->fun_call_expr->lineno = retval->lineno;
      retval->fun_call_expr->column = retval->column;
      retval->fun_call_expr->identifier = tmp;

      if (tok->type != RPAREN)
      {
         retval->fun_call_expr->arg_lst = parse_expr_lst();
         match(RPAREN);
      }
      else
      {
         match(RPAREN);
         retval->fun_call_expr->arg_lst = NULL;
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
   retval->column = last_tok->column;

   match(IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;
   sym->column = last_tok->column;

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
   retval->column = last_tok->column;
   retval->symbol = last_tok->string;

   if (tok->type == DOT)
   {
      match(DOT);
      retval->sub = parse_id();
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* id-lst ::= IDENTIFIER COLON ty
 *          | IDENTIFIER COLON ty COMMA id-lst
 */
static list_t *parse_id_lst()
{
   list_t *retval = NULL;

   ENTERING (__FUNCTION__);

   while (1) {
      absyn_id_lst_t *new_ele;
      absyn_id_expr_t *sym;

      MALLOC (new_ele, sizeof(absyn_id_lst_t));
      MALLOC (sym, sizeof(absyn_id_expr_t));

      match(IDENTIFIER);
      new_ele->lineno = last_tok->lineno;
      new_ele->column = last_tok->column;

      sym->symbol = last_tok->string;
      sym->sub = NULL;
      sym->lineno = last_tok->lineno;
      sym->column = last_tok->column;
      new_ele->symbol = sym;

      match(COLON);
      new_ele->ty = parse_ty();

      retval = list_append (retval, new_ele);

      if (in_set (tok, FOLLOW_SET[SET_ID_LST]))
         break;
      else
         match(COMMA);
   }

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
   retval->column = last_tok->column;
   retval->test_expr = parse_expr();
   match(THEN);
   retval->then_expr = parse_expr();
   match(ELSE);
   retval->else_expr = parse_expr();

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
   retval->column = last_tok->column;

   match (IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;
   sym->column = last_tok->column;
   
   retval->symbol = sym;

   match (ASSIGN);
   match (DECL);
   retval->decl_lst = parse_top_decl_lst();
   match (END);

   if (!in_set (tok, FOLLOW_SET[SET_MODULE_DECL]))
      parse_error (tok, FOLLOW_SET[SET_MODULE_DECL]);

   LEAVING(__FUNCTION__);
   return retval;
}

/* module-decl-lst ::= module-decl
 *                   | module-decl module-decl-lst
 */
static list_t *parse_module_decl_lst()
{
   list_t *retval = NULL;

   ENTERING (__FUNCTION__);

   while (1) {
      if (!in_set (tok, FIRST_SET[SET_MODULE_DECL_LST]))
         parse_error (tok, FIRST_SET[SET_MODULE_DECL_LST]);

      retval = list_append (retval, parse_module_decl());

      if (in_set (tok, FOLLOW_SET[SET_MODULE_DECL_LST]))
         break;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* record-assn-lst ::= IDENTIFIER ASSIGN expr
 *                   | IDENTIFIER ASSIGN expr COMMA record-assn-lst
 */
static list_t *parse_record_assn_lst()
{
   list_t *retval = NULL;
   
   ENTERING (__FUNCTION__);

   while (1) {
      absyn_record_assn_t *new_ele;
      absyn_id_expr_t *sym;

      MALLOC (new_ele, sizeof(absyn_record_assn_t));
      MALLOC (sym, sizeof(absyn_id_expr_t));

      match(IDENTIFIER);
      new_ele->lineno = last_tok->lineno;
      new_ele->column = last_tok->column;

      sym->symbol = last_tok->string;
      sym->sub = NULL;
      sym->lineno = last_tok->lineno;
      sym->column = last_tok->column;

      new_ele->symbol = sym;
      match(ASSIGN);
      new_ele->expr = parse_expr();

      retval = list_append (retval, new_ele);

      if (in_set (tok, FOLLOW_SET[SET_RECORD_ASSN_LST]))
         break;
      else
         match(COMMA);
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* record-ref ::= IDENTIFIER PIPE record-ref
 *              | IDENTIFIER
 */
static absyn_id_expr_t *parse_record_ref()
{
   absyn_id_expr_t *retval = NULL;

   ENTERING (__FUNCTION__);
   MALLOC (retval, sizeof (absyn_id_expr_t));

   match(IDENTIFIER);
   retval->lineno = last_tok->lineno;
   retval->column = last_tok->column;
   retval->symbol = last_tok->string;

   if (tok->type == PIPE)
   {
      match(PIPE);
      retval->sub = parse_record_ref();
   }
   else
      retval->sub = NULL;

   LEAVING (__FUNCTION__);
   return retval;
}

/* sym-ref ::= fun-call-or-id
 *           | fun-call-or-id PIPE record-ref
 */
static absyn_expr_t *parse_sym_ref()
{
   absyn_expr_t *tmp, *retval;

   ENTERING (__FUNCTION__);

   tmp = parse_fun_call_or_id();

   if (tok->type == PIPE)
   {
      match(PIPE);

      MALLOC (retval, sizeof(absyn_expr_t));
      MALLOC (retval->record_ref, sizeof(absyn_record_ref_t));

      retval->kind = ABSYN_RECORD_REF;
      retval->lineno = retval->record_ref->lineno = tmp->lineno;
      retval->column = retval->record_ref->column = tmp->column;

      retval->record_ref->rec = tmp;
      retval->record_ref->element = parse_record_ref();
   }
   else
      retval = tmp;

   LEAVING (__FUNCTION__);
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
         retval->column = tok->column;
         retval->module_decl = parse_module_decl();
         break;

      case FUNCTION:
      case TYPE:
      case VAL:
         retval = parse_decl();
         retval->lineno = tok->lineno;
         retval->column = tok->column;
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
static list_t *parse_top_decl_lst()
{
   list_t *retval = NULL;

   ENTERING (__FUNCTION__);

   while (1) {
      if (!in_set (tok, FIRST_SET[SET_TOP_DECL_LST]))
         parse_error (tok, FIRST_SET[SET_TOP_DECL_LST]);

      retval = list_append (retval, parse_top_decl());

      if (in_set (tok, FOLLOW_SET[SET_TOP_DECL_LST]))
         break;
   }

   LEAVING(__FUNCTION__);
   return retval;
}

/* ty ::= BOTTOM
 *      | LIST ty
 *      | LBRACE id-lst RBRACE
 *      | id
 */
static absyn_ty_t *parse_ty()
{
   absyn_ty_t *retval;

   ENTERING (__FUNCTION__);
   MALLOC(retval, sizeof(absyn_ty_t));

   switch (tok->type) {
      case BOTTOM:
         match(BOTTOM);
         retval->kind = ABSYN_TY_BOTTOM;
         retval->lineno = last_tok->lineno;
         retval->column = last_tok->column;
         break;

      case IDENTIFIER:
         retval->kind = ABSYN_TY_ID;
         retval->identifier = parse_id();
         retval->lineno = retval->identifier->lineno;
         retval->column = retval->identifier->column;
         break;

      case LBRACE:
         match(LBRACE);
         retval->lineno = last_tok->lineno;
         retval->column = last_tok->column;
         retval->kind = ABSYN_TY_RECORD;
         retval->record = parse_id_lst();
         match(RBRACE);
         break;
         
      case LIST:
         match(LIST);
         retval->lineno = last_tok->lineno;
         retval->column = last_tok->column;
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
   retval->column = last_tok->column;

   match(IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;
   sym->column = last_tok->column;

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
   retval->column = last_tok->column;

   match(IDENTIFIER);
   sym->symbol = last_tok->string;
   sym->sub = NULL;
   sym->lineno = last_tok->lineno;
   sym->column = last_tok->column;

   retval->symbol = sym;

   match(COLON);
   retval->ty = parse_ty();

   match(ASSIGN);
   retval->init = parse_expr();

   LEAVING(__FUNCTION__);
   return retval;
}

/* vim: set tags=../tags: */
