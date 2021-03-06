/* << ACE >>

   -- Amiga BASIC Compiler --

   ** Parser: library and machine code function handling **
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
   6th,7th,22nd December 1992,
   28th February 1993,
   12th April 1993,
   16th,17th December 1993,
   2nd,3rd,13th January 1994,
   12th June 1994,
   5th,20th,28th August 1994,
   1st,10th September 1994,
   8th,9th March 1995
 */

#include "acedef.h"
#include <string.h>

#include <stdlib.h>
#include <ctype.h>
//#include <clib/dos_protos.h>

/* locals */
static char *reg[] =
        {"  ", "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",
         "a0", "a1", "a2", "a3", "a4", "a5"};

/* externals */
extern int sym;
extern int lastsym;
extern int typ;
extern int lev;
extern char id[MAXIDSIZE];
extern char ut_id[MAXIDSIZE];
extern char tempshortname[80];
extern char templongname[80];
extern char stringval[MAXSTRLEN];
extern SYM *curr_item;
extern ACELIBS acelib[NUMACELIBS];
extern ACELIBS otherlib[NUMLIBS];
extern char librarybase[MAXIDSIZE + 6];
extern char libraryname[MAXIDSIZE + 20];
extern char bmapname[MAXIDSIZE + 20];
extern BOOL restore_a4;
extern BOOL restore_a5;
extern BOOL early_exit;

/* ------------------------- */
/* Library Function routines */
/* ------------------------- */

BYTE check_for_ace_lib(char *libname) {
    BYTE c = 0;
/* 
   ** If libname is a library used by ACE 
   ** open & close it in the normal way.
 */
    //printf("libname=%s\n",libname);
    while (acelib[c].name[0] != '\0' && strcmp(acelib[c].name, "SENTINEL") != 0) {
        //printf("Checking against %s\n", acelib[c].name);
        if (strcasecmp(acelib[c].name, libname) == 0) {
            enter_XREF(acelib[c].base);
            return (c);
        }
        c++;
    }
    return (NEGATIVE);
}

void enter_new_library(char *libname) {
    int cc = 0;
/*
   ** Enter a library into the list 
   ** for future reference by function
   ** declarations which don't specify
   ** the library to be used.
 */

    /*
       ** Find libname, first free or maximum entry.
       ** If it's already in the list, libname will
       ** be ignored.
     */
    while (otherlib[cc].name[0] != '\0' &&
           strcmp(otherlib[cc].name, libname) != 0 &&
           strcmp(otherlib[cc].name, "SENTINEL") != 0)
        cc++;

    /* maximum? */
    if (strcmp(otherlib[cc].name, "SENTINEL") == 0)
        _error(76);        /* can't add another library */
    else if (otherlib[cc].name[0] == '\0') {
        /* add a new entry */
        strcpy(otherlib[cc].name, libname);
        make_library_base(libname);
        strcpy(otherlib[cc].base, librarybase);
    }
}

void make_library_base(char *libname) {
    strcpy(librarybase, "_\0");
    strcat(librarybase, libname);
    strcat(librarybase, "Base");
    enter_BSS(librarybase, "ds.l 1");
}

BOOL endsWith(const char *str, const char *end) {
    int l1 = strlen(str);
    int l2 = strlen(end);
    if (l2 > l1)
        return FALSE;
    if (strcmp(str + l1 - l2, end))
        return FALSE;
    return TRUE;
}

void make_library_name(char *libname) {

    strcpy(libraryname, libname);
    if (!endsWith(libraryname, ".library"))
        strcat(libraryname, ".library");
    //fprintf(stderr,"%s -> %s\n",libname,libraryname);
}

void make_bmap_name(char *libname) {
    char lowCase[128];
    char *path;

    //path = "/home/alex/dev/ace/bmaps/";

  path=getenv("ACE_Basic");
  if (!path) {
    fprintf(stderr,"\n\n\nPlease define ACE_Basic to show the path where ACE's bmaps are...\n"
    		"(i.e. export ACE_Basic=\"/boo/foo/\" No need to escape spaces)\n"
    );
    exit(20);
  }
    char *d = lowCase;
    char *s = libname;
    while ((*d++ = tolower(*s++))) {}
    strcpy(bmapname, path);
    strcat(bmapname,"bmaps/");
    strcat(bmapname, lowCase);
    strcat(bmapname, ".bmap");
}

void get_libname(char *libname, char *ut_libname) {
    char *tmp, *ut_tmp;

    /* get raw library name (uppercase and untouched) */
    if (sym == ident) {
        strcpy(ut_libname, ut_id);
        strcpy(libname, id);
    } else {
        strcpy(ut_libname, stringval);
        //strcpy (libname, strupr (stringval));
        strcpy(libname, stringval);
    }

    /* chop off extension if ".bmap" or ".library" */
    tmp = libname;
    ut_tmp = ut_libname;
    while (*tmp && *tmp != '.') {
        tmp++;
        ut_tmp++;
    }
    if (*tmp == '.' &&
        (strcmp(tmp, ".bmap") == 0 || strcmp(tmp, ".library") == 0)) {
        *tmp = '\0';
        *ut_tmp = '\0';
    }
}

void library(void) {
    char libname[MAXIDSIZE], ut_libname[MAXIDSIZE];
    char lab[80], lablabel[80];

    /* open or close a shared library */

    insymbol();
    if (sym == closesym)
        closelibrary();
    else {
        if (sym != ident && sym != stringconst)
            _error(7);        /* identifier expected */
        else {
            get_libname(libname, ut_libname);    /* without extension! */

            if (check_for_ace_lib(libname) == NEGATIVE) {
                make_library_name(ut_libname);    /* exec names are case sensitive */
                make_string_const(libraryname);
                gen("move.l", "(sp)+", "a1");    /* address of library name in a1 */
                gen("jsr", "_open_library", "  ");
                make_library_base(libname);
                gen("move.l", "d0", librarybase);
                gen("cmpi.l", "#0", "d0");
                make_label(lab, lablabel);
                gen("bne.s", lab, "  ");
                gen("jmp", "_EXIT_PROG", "  ");    /* quit program if can't open library */
                gen(lablabel, "  ", "  ");
                enter_XREF("_open_library");

                /* enter new library info' into "other libraries" list */
                enter_new_library(libname);
            }
        }
        insymbol();
    }
}

void closelibrary(void) {
    char libname[MAXIDSIZE], ut_libname[MAXIDSIZE];
    int cc;
    /* close one or more shared libraries */

    insymbol();
    if (sym != ident && sym != stringconst) {
        /*
           ** Close all open shared libraries not
           ** normally closed by ACE at the end of
           ** the program run.
         */
        cc = 0;
        while (otherlib[cc].name[0] != '\0' &&
               strcmp(otherlib[cc].name, "SENTINEL") != 0) {
            gen("move.l", otherlib[cc].base, "a1");    /* library base in a1 */
            gen("jsr", "_close_library", "  ");
            gen("move.l", "#0", otherlib[cc].base);
            enter_XREF("_close_library");
            cc++;
        }
    } else {
        /* close a single shared library */
        get_libname(libname, ut_libname);    /* without extension! */

        if (check_for_ace_lib(libname) == NEGATIVE) {
            make_library_base(libname);
            gen("move.l", librarybase, "a1");    /* library base in a1 */
            gen("jsr", "_close_library", "  ");
            gen("move.l", "#0", librarybase);
            enter_XREF("_close_library");
        }
        insymbol();
    }
}

void remove_qualifier(char *funcname) {
/* remove any trailing data type qualifier (%&#!$) @=& [=! */
    int cc = 0;

    /* find end of string */
    while (funcname[cc] != '\0')
        cc++;
    /* is the last character a qualifier? */
    if ((funcname[cc - 1] == '%') || (funcname[cc - 1] == '@') || (funcname[cc - 1] == '#')
        || (funcname[cc - 1] == '[') || (funcname[cc - 1] == '$'))
        funcname[cc - 1] = '\0';
}


BOOL found_func(char *libname, char *ut_funcname, SYM *declared_func) {
    /*
       ** Search for function in bmap file and record register usage
       ** and library base offset in symbol table entry for function.
     */

    make_bmap_name(libname);

    if (search_func(bmapname, ut_funcname, declared_func)) {
        /* store library name */
        declared_func->libname = (char *) sym_alloc(MAXIDSIZE + 8, MEMF_ANY);
        if (declared_func->libname == NULL) {
            puts("Can't allocate memory for library name in symbol table!");
            early_exit = TRUE;
            kill_all_lists();
            cleanup();
        } else {
            //fprintf(stderr,"Alkis...this is it: %s %s %s\n",bmapname,libname,ut_funcname);
            /* found the function */
            strcpy(declared_func->libname, libname);
            return (TRUE);
        }
    } else {
        //fprintf(stderr,"Alkis...this is it: %s %s %s\n",bmapname,libname,ut_funcname);
        /* didn't find the function */
        return (FALSE);
    }
}

void declare(void) {
/* - declare a library function -- NOT optional. 
   - declare an external function -- NOT optional.
   - declare a forward reference to a SUB. 
   - declare an instance of a structure. 
 */

    char funcname[MAXIDSIZE], ut_funcname[MAXIDSIZE];
    char libname[MAXIDSIZE], ut_libname[MAXIDSIZE];
    char extfuncname[MAXIDSIZE + 1];
    int oldlevel, cc;
    int functype = undefined;
    SYM *declared_func;
    BOOL found;

    /* all functions and structures must be declared at level ZERO */

    oldlevel = lev;
    lev = ZERO;

    insymbol();

    if (sym == subsym) {
        lev = oldlevel;
        forward_ref();
        return;
    }

    if (sym == structsym) {
        lev = oldlevel;
        declare_structure();
        return;
    }

    if (sym != functionsym)
        _error(47);
    else {
        insymbol();

        /* type identifiers */
        if (sym == shortintsym || sym == longintsym || sym == addresssym ||
            sym == singlesym || sym == stringsym) {
            switch (sym) {
                case shortintsym:
                    functype = shorttype;
                    break;
                case longintsym:
                    functype = longtype;
                    break;
                case addresssym:
                    functype = longtype;
                    break;
                case singlesym:
                    functype = singletype;
                    break;
                case stringsym:
                    functype = stringtype;
                    break;
            }
            insymbol();
        }

        if (sym != ident)
            _error(7);
        else {
            /* get the function's name */
            strcpy(funcname, id);
            strcpy(ut_funcname, ut_id);    /* case sensitive version for search! */
            remove_qualifier(funcname);
            remove_qualifier(ut_funcname);
            if (functype == undefined)
                functype = typ;

            /* get the LIBRARY symbol and/or parameter list */
            insymbol();

            /* parse parameter list? */
            if (sym == lparen) {
                do {
                    insymbol();

                    if (sym == rparen && lastsym == lparen)
                        break;    /* FuncName() */

                    /* type indentifier? */
                    if (sym == shortintsym || sym == longintsym || sym == addresssym ||
                        sym == singlesym || sym == stringsym)
                        insymbol();

                    /* parameter */
                    if (sym != ident)
                        _error(7);
                    else
                        insymbol();
                } while (sym == comma);

                if (sym != rparen)
                    _error(9);
                else
                    insymbol();
            }

            /* EXTERNAL or LIBRARY function? */
            if (sym == externalsym) {
                if (ut_funcname[0] != '_') {
                    strcpy(extfuncname, "_\0");
                    strcat(extfuncname, ut_funcname);
                } else
                    strcpy(extfuncname, ut_funcname);

                enter(extfuncname, functype, extfunc, 0);
                enter_XREF(extfuncname);

                insymbol();
            } else if (sym != librarysym)
                _error(48);
            else {
                enter(funcname, functype, function, 0);    /* enter into symbol table */
                declared_func = curr_item;

                insymbol();

                if (sym != ident && sym != stringconst) {
                    /*
                       ** Search all bmap files found in acelib and otherlib arrays
                       ** for function since no library was specified.
                     */

                    found = FALSE;

                    /* acelib array */
                    cc = 0;
                    while (!found && acelib[cc].name[0] != '\0' &&
                           strcmp(acelib[cc].name, "SENTINEL") != 0) {
                        found = found_func(acelib[cc].name, ut_funcname, declared_func);
                        cc++;
                    }

                    if (found) {
                        lev = oldlevel;
                        return;
                    }

                    /* otherlib array */
                    cc = 0;
                    while (!found && otherlib[cc].name[0] != '\0' &&
                           strcmp(otherlib[cc].name, "SENTINEL") != 0) {
                        found = found_func(otherlib[cc].name, ut_funcname, declared_func);
                        cc++;
                    }

                    if (!found)
                        _error(49);
                } else {
                    /*
                       ** Search specified library (bmap file) for the declared function.
                     */
                    get_libname(libname, ut_libname);    /* without extension! */

                    if (!found_func(libname, ut_funcname, declared_func))
                        _error(49);

                    insymbol();
                }
            }
        }
    }
    lev = oldlevel;
}

void load_func_params(SYM *func_item) {
/* 
   ** Parse a shared library function's actual parameter-list 
   ** and call the function.
 */
    SHORT i, n;
    char ptemp[14][80];

    if (sym != lparen) {
        _error(14);
        return;
    } else {
        /* get parameters */
        i = 0;
        do {
            insymbol();

            /*
               ** Make ALL parameters longints!
             */
            if (expr() == shorttype)
                make_long();

            /*
               ** Store parameter information temporarily
               ** since expression evaluation may corrupt
               ** data in registers later if loaded now.
             */
            make_temp_long();
            strcpy(ptemp[i], templongname);    /* later -> move.l srctemp,addrbuf */

            /* store it */
            gen("move.l", "(sp)+", templongname);

            i++;
        } while ((i < func_item->no_of_params) && (sym == comma));

        if ((i < func_item->no_of_params) || (sym == comma))
            _error(39);        /* parameter count mismatch - too few or too many. */
        else {
            /* load parameters into regs */
            for (n = 0; n < func_item->no_of_params; n++) {
                /* does reg (a4 or a5 ONLY) need to be preserved? */
                if (func_item->reg[n] == 13)    /* a4 */
                {
                    restore_a4 = TRUE;
                    gen("move.l", "a4", "_a4_temp");
                    enter_BSS("_a4_temp:", "ds.l 1");
                } else if (func_item->reg[n] == 14)        /* a5 */
                {
                    restore_a5 = TRUE;
                    gen("move.l", "a5", "_a5_temp");
                    enter_BSS("_a5_temp:", "ds.l 1");
                }

                /*
                   ** Store value in register.
                 */
                gen("move.l", ptemp[n], reg[func_item->reg[n]]);
            }
        }

        if (sym != rparen)
            _error(9);
    }
}

/* ----------------- */
/* Machine Code CALL */
/* ----------------- */

void load_mc_params(SYM *sub_ptr) {
    SHORT i, n;
    int ptype;
    char p_temp[MAXPARAMS][80];
    int p_type[MAXPARAMS];

    /*
       ** Store parameters in stack space of MC subroutine
       ** which is about to be CALLed ala C parameter passing
       ** convention.
     */

    i = 0;
    do {
        insymbol();
        ptype = expr();

        /* store parameters temporarily */
        if (ptype == shorttype) {
            /* save parameter type */
            p_type[i] = shorttype;    /* not DATA type but STORE type (2 or 4 bytes) */

            /* create a temporary store */
            make_temp_short();
            strcpy(p_temp[i], tempshortname);    /* later -> move.w srctemp,-(sp) */

            /* store it */
            gen("move.w", "(sp)+", tempshortname);
        } else
            /* long,single,string or array */
        {
            /* save parameter type */
            p_type[i] = longtype;    /* storage requirement is 4 bytes */

            /* create a temporary store */
            make_temp_long();
            strcpy(p_temp[i], templongname);

            /* store it */
            gen("move.l", "(sp)+", templongname);
        }

        i++;
    } while ((i < MAXPARAMS) && (sym == comma));

    /* push parameters onto stack ala C (see AmigaBASIC manual 6-11) */
    for (n = i - 1; n >= 0; n--) {
        if (p_type[n] == shorttype) {
            gen("move.w", p_temp[n], "-(sp)");    /* short */
            sub_ptr->p_type[n] = shorttype;
        } else {
            gen("move.l", p_temp[n], "-(sp)");    /* long,string,single,array */
            sub_ptr->p_type[n] = longtype;
        }
    }

    sub_ptr->no_of_params = i;    /* record number of parameters pushed! */

    if (sym != rparen)
        _error(9);
}
