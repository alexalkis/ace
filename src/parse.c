/* << ACE >>

   -- Amiga BASIC Compiler --

   ** Parser: main module **
   ** Copyright (C) 1998 David Benn
   ** 
   ** This program is free software; you can redistribute it and/or
   ** modify it under the terms of the GNU General Public License
   ** as published by the Free Software Foundation; either version 2
   ** of the License, or (at your option) any later version.
   **
   ** This program is distributed in the hope that it will be useful,
   ** but WITHOUT ANY WARRANTY; without even the implied warranty of
   ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   ** GNU General Public License for more details.
   **
   ** You should have received a copy of the GNU General Public License
   ** along with this program; if not, write to the Free Software
   ** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

   Author: David J Benn
   Date: 26th October-30th November, 1st-13th December 1991,
   14th,20th-27th January 1992, 
   2nd-17th, 21st-29th February 1992, 
   1st,13th,14th,22nd,23rd March 1992,
   21st,22nd April 1992,
   2nd,3rd,11th,15th,16th May 1992,
   7th,8th,9th,11th,13th,14th,28th,29th,30th June 1992,
   2nd-8th,14th-19th,26th-29th July 1992,
   1st-3rd,7th,8th,9th August 1992,
   6th,7th,21st,22nd December 1992,
   13th,30th January 1993,
   2nd,6th,11th,14th,15th,28th February 1993,
   7th,24th March 1993,
   6th,13th,14th,30th June 1993,
   1st,10th July 1993,
   5th,26th September 1993,
   25th October 1993,
   15th-18th November 1993,
   26th,27th December 1993,
   2nd,5th,7th,9th,13th,14th January 1994,
   7th,26th,27th February 1994,
   4th,13th,30th April 1994,
   14th May 1994,
   12th,14th,21st,22nd,25th June 1994,
   10th,14th,24th July 1994,
   14th,19th August 1994,
   11th,12th September 1994,
   5th,11th March 1995,
   8th August 1995,
   6th October 1995,
   10th March 1996
 */

#include "acedef.h"
#include <string.h>
#include <stdlib.h>
//#include <clib/exec_protos.h>

/* externals */
extern char *srcfile, *destfile;
extern int sym;
extern int typ;
extern int lev;
extern int errors;
extern char id[MAXIDSIZE];
extern SYM *curr_item;
extern CODE *curr_code;
extern BOOL end_of_source;
extern FILE *dest;
extern int addr[2];
extern char exit_sub_name[80];
extern BOOL early_exit;
extern int exitvalue;
extern char *rword[];
extern BOOL break_opt;
extern BOOL optimise_opt;
extern BOOL make_icon;
extern BOOL error_log;
extern BOOL asm_comments;
extern BOOL list_source;
extern BOOL wdw_close_opt;
extern BOOL module_opt;
extern BOOL cli_args;
extern BOOL mathffpused;
extern BOOL mathtransused;
extern BOOL dosused;
extern BOOL gfxused;
extern BOOL intuitionused;
extern BOOL translateused;
extern BOOL narratorused;
extern BOOL ontimerused;
extern BOOL iffused;
extern BOOL basdatapresent;
extern BOOL readpresent;

/* external functions */
char *version (void);

/* functions */
void parse (void)
{
  lev = ZERO;
  addr[lev] = 0;
  new_symtab ();
  create_lists ();

  insymbol ();
  block ();

  undef_label_check ();
  kill_symtab ();
}


void show_title (void)
{
  printf ("ACE Amiga BASIC Compiler version %s, copyright ", version ());
  putchar (169);		/* copyright symbol */
  puts (" 1991-1996 David Benn.");
}

void usage (void)
{
  printf ("usage: ACE [words | -bcEilmOw] <sourcefile>[.b[as]]\n");
  printf (" ACE -b check for CTRL-C by user\n");
  printf (" ACE -c include each line as a comment\n");
  printf (" ACE -E create the ace.err file in current directory\n");
  printf (" ACE -i create an icon for the executable\n");
  printf (" ACE -l show each line as it's compiled\n");
  printf (" ACE -m create a linkable module\n");
  printf (" ACE -O optimize the assembly source code\n");
  printf (" ACE -w check for window close gadgets\n");
}

BOOL check_options (char *opt)
{
  BOOL legalopt = TRUE;

  if (*opt != '-')
    return (FALSE);
  /* extract the options */
  opt++;
  if (*opt == '\0')
    legalopt = FALSE;
  else
    while ((*opt != '\0') && (legalopt))
      {
	if (*opt == 'b')
	  break_opt = TRUE;
	else if (*opt == 'O')
	  optimise_opt = TRUE;
	else if (*opt == 'i')
	  make_icon = TRUE;
	else if (*opt == 'E')
	  error_log = TRUE;
	else if (*opt == 'c')
	  asm_comments = TRUE;
	else if (*opt == 'l')
	  list_source = TRUE;
	else if (*opt == 'm')
	  module_opt = TRUE;
	else if (*opt == 'w')
	  wdw_close_opt = TRUE;
	else
	  legalopt = FALSE;
	opt++;
      }

  return (legalopt);
}

void ctrl_c_break_test (void)
{
  /*
  if (SetSignal (0L, SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C)
    {
      puts ("\n*** Break: ACE terminating.");
      exit (5);
    }
    */
}

void dump_reserved_words (void)
{
  int i;

  printf ("\nAmigaBASIC RESERVED WORDS: %ld\n\n", (xorsym - abssym) + 1);

  for (i = abssym; i <= xorsym; i++)
    {
      printf ("%s\n", rword[i]);
      ctrl_c_break_test ();
    }

  printf ("\nACE-SPECIFIC RESERVED WORDS: %ld\n\n", (ycorsym - addresssym) + 1);

  for (i = addresssym; i <= ycorsym; i++)
    {
      printf ("%s\n", rword[i]);
      ctrl_c_break_test ();
    }
}

int main(int argc, char *argv[])
{
  char *tmparg;

  show_title ();

  /* 
     ** - get args and compiler switches.
     ** - open source and destination files. 
   */
  if ((argc == 1) || (argc > 3))
    {
      usage ();
      exit (10);
    }				/* arg count mismatch */

  /* send reserved words to stdout then quit? */
  tmparg = (char *) malloc (strlen (argv[1]) + 1);
  if (tmparg == NULL)
    {
      puts ("Unable to allocate temporary argument buffer!");
      exit (10);
    }
  else
    {
      strcpy (tmparg, argv[1]);

      if (strcmp (strupr (tmparg), "WORDS") == 0)
	{
	  dump_reserved_words ();
	  if (tmparg)
	    free (tmparg);
	  exit (0);
	}
      else if (tmparg)
	free (tmparg);
    }

  open_shared_libs ();

  /* 
     ** compile program 
   */
  if (argc == 2)
    {
      /* source file, no options */
      open_files (argv[1]);
      setup ();
      compile (srcfile, destfile);
    }
  else
    {
      /* options plus source file */
      if (!check_options (argv[1]))
	{
	  usage ();
	  close_shared_libs ();
	  exit (10);
	}			/* illegal options */
      open_files (argv[2]);
      setup ();
      compile (srcfile, destfile);
    }

  cleanup ();
}
