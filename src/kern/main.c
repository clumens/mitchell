/* The main file of the mitchell kernel, which controls the entire
 * compilation process.
 *
 * $Id: main.c,v 1.22 2004/12/07 00:27:14 chris Exp $
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
#include "error.h"
#include "memory.h"
#include "parse.h"
#include "semant.h"
#include "version.h"

/* What we want getopt_long to return. */
typedef enum { OPT_HELP = 1000, OPT_VERBOSE_HELP, OPT_VERSION,
               OPT_IDEBUG_PARSER, OPT_IDUMP_ABSYN,
               OPT_IDUMP_SYMTABS } config_vals_t;

/* The command line arguments we accept. */
static char *shortopts = "hv";

static struct option longopts[] = {
   /* Help options */
   { "help", 0, NULL, OPT_HELP },
   { "verbose-help", 0, NULL, OPT_VERBOSE_HELP },
   { "version", 0, NULL, OPT_VERSION },

   /* Internal compiler options */
   { "Idebug-parser", 1, NULL, OPT_IDEBUG_PARSER },
   { "Idump-absyn", 2, NULL, OPT_IDUMP_ABSYN },
   { "Idump-symtabs", 0, NULL, OPT_IDUMP_SYMTABS },

   { 0, 0, 0, 0 }
};

/* Stash all the command line arguments in here. */
compiler_config_t compiler_config = { .debug.parser_debug = 0,
                                      .debug.dump_absyn = 0,
                                      .debug.dump_symtabs = 0};

static void help_internal_debug ()
{
   printf ("Internal Debugging Options:\n");
   printf ("-Idebug-parser=N\tSet debugging output level for the tokenizer"
           " and parser\n");
   printf ("-Idump-absyn[=file]\tDump the abstract syntax tree to 'file',"
           " or <infile>.ast\n\t\t\tby default\n");
   printf ("-Idump-symtabs\t\tDump the symbol tables on exit from a level of "
           "scope\n");
}

static void verbose_help (const char *progname)
{
   printf ("usage:  %s <infile>\n", progname);
   help_internal_debug();
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

         case OPT_IDEBUG_PARSER:
            if (optarg)
               compiler_config.debug.parser_debug = atoi(optarg);
            else
            {
               printf ("-Idebug-parser requires an argument\n");
               exit(1);
            }

            break;

         case OPT_IDUMP_ABSYN:
            /* If no file was provided, we have to delay coming up with the
             * absyn outfile until after we know the name of the input file.
             */
            compiler_config.debug.dump_absyn = 1;
            if (optarg)
               compiler_config.debug.absyn_outfile = strdup(optarg);
            else
               compiler_config.debug.absyn_outfile = NULL;
            break;

         case OPT_IDUMP_SYMTABS:
            compiler_config.debug.dump_symtabs = 1;
            break;

         /* getopt already told us what was wrong so only print the help. */
         default:
            help(argv[0]);
            break;
      }
   }

   if (optind+1 == argc)
      compiler_config.filename = strdup (argv[optind]);
   else
      help (argv[0]);

   /* Now that we know the name of the input file, we can do some additional
    * argument processing for things that depend on it.
    */
   if (compiler_config.debug.dump_absyn &&
       compiler_config.debug.absyn_outfile == NULL)
   {
      MALLOC (compiler_config.debug.absyn_outfile,
              strlen(compiler_config.filename)+5);
      compiler_config.debug.absyn_outfile =
         strcpy(compiler_config.debug.absyn_outfile, compiler_config.filename);
      compiler_config.debug.absyn_outfile =
         strcat(compiler_config.debug.absyn_outfile, ".ast");
   }
}

int main (int argc, char **argv)
{
   ast_t *ast;

   /* Grab locale information from the environment. */
   setlocale (LC_ALL, "");

   /* Make sure we are in a UTF-8 aware locale.  If not, bail out.  We have to
    * do this because if the environment is wrong, the tokenizer will explode
    * on reading the source file and the user will get some horrible message.
    */
   if (strncmp (nl_langinfo(CODESET), "UTF-8", 5) != 0)
   {
      USAGE_ERROR("<none>",
                  "Your current locale is not UTF-8 aware.  The mitchell "
                  "compiler requires\n\tthe proper environment settings to be "
                  "able to read source files.  You\n\twill need to set your "
                  "$LANG or $LC_ALL environment variables to a locale\n\t"
                  "which is UTF-8 aware.  A good setting for $LANG might be "
                  "en_US-UTF-8.\n\tExiting.");
      exit(1);
   }

   handle_arguments (argc, argv);
   ast = parse (compiler_config.filename);

   if (compiler_config.debug.dump_absyn)
      print_absyn (ast, &compiler_config);

   check_program (ast);

   return 0;
}

/* vim: set tags=../tags: */
