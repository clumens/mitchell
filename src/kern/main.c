/* The main file of the mitchell kernel, which controls the entire
 * compilation process.
 *
 * $Id: main.c,v 1.29 2005/02/12 17:07:43 chris Exp $
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
#include <getopt.h>
#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "absyn.h"
#include "config.h"
#include "desugar.h"
#include "error.h"
#include "memory.h"
#include "parse.h"
#include "semant.h"
#include "str.h"
#include "version.h"

/* What we want getopt_long to return. */
typedef enum { OPT_HELP = 1000, OPT_VERBOSE_HELP, OPT_VERSION,
               OPT_LAST_PHASE,
               OPT_IDEBUG_PARSER, OPT_IDUMP_ABSYN, OPT_IDUMP_SYMTAB,
               OPT_WERROR } config_vals_t;

/* The command line arguments we accept. */
static char *shortopts = "hv";

static struct option longopts[] = {
   /* Help options */
   { "help", 0, NULL, OPT_HELP },
   { "verbose-help", 0, NULL, OPT_VERBOSE_HELP },
   { "version", 0, NULL, OPT_VERSION },

   /* General options */
   { "last-phase", 1, NULL, OPT_LAST_PHASE },

   /* Internal compiler options */
   { "Idebug-parser", 1, NULL, OPT_IDEBUG_PARSER },
   { "Idump-absyn", 2, NULL, OPT_IDUMP_ABSYN },
   { "Idump-symtab", 2, NULL, OPT_IDUMP_SYMTAB },

   /* Options for dealing with warnings */
   { "Werror", 0, NULL, OPT_WERROR },

   { 0, 0, 0, 0 }
};

/* Stash all the command line arguments in here. */
compiler_config_t cconfig = { .last_phase = 0,
                              .warnings_are_errors = 0,
                              .debug.parser_debug = 0,
                              .debug.dump_absyn = 0,
                              .debug.dump_symtab = 0};

static void help_general ()
{
   printf ("General Options:\n");
   printf ("-last-phase=phase\tStop compilation after the given phase\n");
}

static void help_internal_debug ()
{
   printf ("Internal Debugging Options:\n");
   printf ("-Idebug-parser=N\tSet debugging output level for the tokenizer"
           " and parser\n");
   printf ("-Idump-absyn[=file]\tDump the abstract syntax tree to 'file',"
           " or <infile>.ast\n\t\t\tby default\n");
   printf ("-Idump-symtab[=file]\tDump the symbol tables to 'file',"
           " or <infile>.symtab\n\t\t\tby default\n");
}

static void help_warnings ()
{
   printf ("Warning Options:\n");
   printf ("-Werror\t\t\tTerminate compilation on warnings, as on errors\n");
}

static void verbose_help (const char *progname)
{
   printf ("usage:  %s <infile>\n", progname);
   help_general();
   printf ("\n");
   help_internal_debug();
   printf ("\n");
   help_warnings();
   exit (0);
}

static void help (const char *progname)
{
   printf ("usage:  %s <infile>\n", progname);
   printf ("\t-verbose-help for a list of all command line arguments\n");
   exit (0);
}

static void version (const char *progname)
{
   printf ("%s version %s\n(C) 2004 Chris Lumens\n", MITCHELL_VERSION,
           progname);
   exit (0);
}

static void handle_arguments (int argc, char **argv)
{
   int index, retval;

   if (argc == 1)
      help (argv[0]);

   while ((retval = getopt_long_only (argc, argv, shortopts, longopts,
                                      &index)) != -1)
   {
      switch (retval) {
         case 'h':
         case OPT_HELP:
            help (argv[0]);
            break;

         case OPT_VERBOSE_HELP:
            verbose_help (argv[0]);
            break;

         case 'v':
         case OPT_VERSION:
            version (argv[0]);
            break;

         case OPT_LAST_PHASE:
            if (!optarg)
            {
               ERROR ("-last-phase requires an argument.  See the man page "
                      "for details.");
               exit(1);
            }

            if (strcmp (optarg, "parser") == 0)
               cconfig.last_phase = LAST_PARSER;
            else if (strcmp (optarg, "typecheck") == 0)
               cconfig.last_phase = LAST_TYPECHECK;
            else if (strcmp (optarg, "desugar-case") == 0)
               cconfig.last_phase = LAST_DESUGAR_CASE;
            else if (strcmp (optarg, "desugar-decl") == 0)
               cconfig.last_phase = LAST_DESUGAR_DECL;
            else
            {
               ERROR ("Invalid option supplied to -last-phase.  See the man "
                      "page for details.");
               exit(1);
            }

         case OPT_IDEBUG_PARSER:
            if (optarg)
               cconfig.debug.parser_debug = atoi(optarg);
            else
            {
               ERROR ("-Idebug-parser requires an argument.  See the man page "
                      "for details.");
               exit(1);
            }

            break;

         case OPT_IDUMP_ABSYN:
            /* If no file was provided, we have to delay coming up with the
             * absyn outfile until after we know the name of the input file.
             */
            cconfig.debug.dump_absyn = 1;
            if (optarg)
               cconfig.debug.absyn_outfile = strdup(optarg);
            else
               cconfig.debug.absyn_outfile = NULL;
            break;

         case OPT_IDUMP_SYMTAB:
            /* Same comment as for OPT_IDUMP_SYMTAB */
            cconfig.debug.dump_symtab = 1;
            if (optarg)
               cconfig.debug.symtab_outfile = strdup(optarg);
            else
               cconfig.debug.symtab_outfile = NULL;
            break;

         case OPT_WERROR:
            cconfig.warnings_are_errors = 1;
            break;

         /* getopt already told us what was wrong so only print the help. */
         default:
            help(argv[0]);
            break;
      }
   }

   if (optind+1 == argc)
      cconfig.filename = strdup (argv[optind]);
   else
      help (argv[0]);

   /* Now that we know the name of the input file, we can do some additional
    * argument processing for things that depend on it.
    */
   if (cconfig.debug.dump_absyn && cconfig.debug.absyn_outfile == NULL)
   {
      MALLOC (cconfig.debug.absyn_outfile, strlen(cconfig.filename)+5);
      cconfig.debug.absyn_outfile = strcpy(cconfig.debug.absyn_outfile,
                                           cconfig.filename);
      cconfig.debug.absyn_outfile = strcat(cconfig.debug.absyn_outfile, ".ast");
   }
 
   if (cconfig.debug.dump_symtab && cconfig.debug.symtab_outfile == NULL)
   {
      MALLOC (cconfig.debug.symtab_outfile, strlen(cconfig.filename)+8);
      cconfig.debug.symtab_outfile = strcpy(cconfig.debug.symtab_outfile,
                                            cconfig.filename);
      cconfig.debug.symtab_outfile = strcat(cconfig.debug.symtab_outfile,
                                            ".symtab");
   }
}

int main (int argc, char **argv)
{
   ast_t *ast, *simple_ast;

   /* Grab locale information from the environment. */
   setlocale (LC_ALL, "");

   /* Make sure we are in a UTF-8 aware locale.  If not, bail out.  We have to
    * do this because if the environment is wrong, the tokenizer will explode
    * on reading the source file and the user will get some horrible message.
    */
   if (strncmp (nl_langinfo(CODESET), "UTF-8", 5) != 0)
   {
      ERROR("Your current locale is not UTF-8 aware.  The mitchell compiler "
            "requires\n\tthe proper environment settings to be able to read "
            "source files.  You\n\twill need to set your $LANG or $LC_ALL "
            "environment variables to a locale\n\twhich is UTF-8 aware.  A "
            "good setting for $LANG might be en_US-UTF-8.\n\tExiting.");
      exit(1);
   }

   handle_arguments (argc, argv);
   ast = parse (cconfig.filename);

   if (cconfig.debug.dump_absyn)
      print_absyn (ast, &cconfig, "Initial abstract syntax tree");

   if (cconfig.last_phase == LAST_PARSER)
      return 0;

   check_program (ast);

   if (cconfig.last_phase == LAST_TYPECHECK)
      return 0;

   simple_ast = desugar_ast (ast);
   if (cconfig.debug.dump_absyn)
      print_absyn (simple_ast, &cconfig, "Simplified abstract syntax tree");

   return 0;
}

/* vim: set tags=../tags: */
