/* The main file of the mitchell kernel, which controls the entire
 * compilation process.
 *
 * $Id: main.c,v 1.10 2004/10/20 14:12:41 chris Exp $
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
#include <getopt.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "parse.h"

/* What we want getopt_long to return. */
typedef enum { OPT_HELP = 100, OPT_VERBOSE_HELP, OPT_VERSION,
               OPT_IDEBUG_PARSER } config_vals_t;

/* The command line arguments we accept. */
static char *shortopts = "hv";

static struct option longopts[] = {
   /* Help options */
   { "help", 0, NULL, OPT_HELP },
   { "verbose-help", 0, NULL, OPT_VERBOSE_HELP },
   { "version", 0, NULL, OPT_VERSION },

   /* Internal compiler options */
   { "Idebug-parser", 1, NULL, OPT_IDEBUG_PARSER },

   { 0, 0, 0, 0 }
};

/* Stash all the command line arguments in here. */
compiler_config_t compiler_config = { .debug.parser_debug = 0 };

static void help_internal_debug ()
{
   printf ("Internal Debugging Options:\n");
   printf ("-Idebug-parser=N\t\tSet debugging output level for the tokenizer and parser\n");
}

static void verbose_help (const char *progname)
{
   printf ("usage:  %s <filename>\n", progname);
   help_internal_debug();
   exit (0);
}

static void help (const char *progname)
{
   printf ("usage:  %s <filename>\n", progname);
   printf ("\t-verbose-help for a list of all command line arguments\n");
   exit (0);
}

static void version (const char *progname)
{
   printf ("%s version WHATEVER\n(C) 2004 Chris Lumens\n", progname);
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
}

int main (int argc, char **argv)
{
   /* Grab locale information from the environment. */
   setlocale (LC_ALL, "");

   handle_arguments (argc, argv);
   parse (compiler_config.filename);

   return 0;
}

/* vim: set tags=../tags: */
