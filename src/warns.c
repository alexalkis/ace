#include "acedef.h"
#include <string.h>
#include <stdlib.h>
//#include <clib/dos_protos.h>
//#include <clib/intuition_protos.h>

/* local variables */
extern struct Remember *GenRememberList;
extern struct Remember *SymRememberList[2];

/* Externals */

extern int lev;
extern int sym;
extern int typ;
extern int errors;
extern int exitvalue;
extern int addr[2];
extern char string_const_start[7];
extern char string_const_end[4];
extern char id[MAXIDSIZE];
extern char exit_sub_name[80];
extern FILE *dest;
extern SYM *curr_item;
extern CODE *curr_code;
extern BOOL early_exit;
extern BOOL optimise_opt;
extern BOOL module_opt;
extern BOOL cli_args;
extern BOOL translateused;
extern BOOL mathffpused;
extern BOOL mathtransused;
extern BOOL gfxused;
extern BOOL intuitionused;
extern BOOL iffused;
extern BOOL ontimerused;
extern BOOL narratorused;
extern BOOL basdatapresent;
extern BOOL readpresent;
extern BOOL make_icon;
extern BOOL end_of_source;
extern BOOL debug_msg;

/* locals */
static char *frame_ptr[] = {"(a4)", "(a5)"};

/* functions  assign.c */
void make_data_const(char *string) {
    char *strbuf;

    /* actual string constant */
    strbuf = (char *) alloc(strlen(string) + 10L, MEMF_ANY);
    /* +10 is for string_const_start/end (9) & '\0' */
    strcpy(strbuf, string_const_start);
    strcat(strbuf, string);
    strcat(strbuf, string_const_end);
    enter_BASDATA(strbuf);
    /*FreeMem(strbuf,strlen(string)+10); */
}

BOOL search_func(char *bmap, char *func, SYM *declared_func) {
    /*
     ** Search for function in .bmap file, recording register usage
     ** and library base offset in symbol table entry for function.
     */

    unsigned char *buf;
    //struct FileHandle *f;
    FILE *f;
    unsigned char ch, name[MAXIDSIZE];
    long bmap_size, count, cc, rc;
    SHORT offset = 0;
    BOOL found = FALSE;


    f = fopen(bmap, "r");

    if (!f) {
        //fprintf(stderr,"Can't open %s\n",bmap);
        _error(50);
        return FALSE;
    }

    fseek(f, 0, SEEK_END);
    bmap_size = ftell(f);
    fseek(f, 0, SEEK_SET);    //alkis added this one

    /* read whole bmap file into a buffer */
    buf = (unsigned char *) alloc(bmap_size, MEMF_ANY);

    //int t = 0;
    int t = (int) fread(buf, 1, bmap_size, f);
    //memset(buf, bmap_size,1);

    if (t != bmap_size)
        fprintf(stderr, "Weird....\n");

    fclose(f);

    count = 0; /* start of buffer */

    while (!found && count < bmap_size) {
        /* build function name */
        cc = 0;
        while (count < bmap_size && cc < MAXIDSIZE) {
            ch = name[cc++] = buf[count++];
            if (ch == '\0')
                break; /* exit loop when EOS reached */
        }

        name[cc - 1] = '\0'; /* make sure we have an EOS symbol (may exit early) */

        if (strcmp((char *) name, func) != 0) {
            /* skip 2-byte offset */
            count += 2;
            /* skip to next name */
            while (count < bmap_size && (ch = buf[count++]) != '\0');
        } else
            found = TRUE; /* that's it! -> get the info' */
    }

    if (!found)
        return (FALSE);
    //fprintf(stderr, "Found %s in %s\n", func, bmap);
    /* get library base offset (2 bytes) */
    if (count < bmap_size) {
        ch = buf[count++];
        offset += ch * 256;
    } else
        return (FALSE);

    if (count < bmap_size) {
        ch = buf[count++];
        offset += ch;
    } else
        return (FALSE);

    declared_func->address = (SHORT) offset; /* record library base offset */

    /* get n bytes of register data */
    declared_func->reg = (UBYTE *) sym_alloc(15, MEMF_ANY);
    if (declared_func->reg == NULL) {
        puts("Can't allocate memory for function register info!");
        early_exit = TRUE;
        kill_all_lists();
        cleanup();
    }

    rc = 0;
    while (count < bmap_size && (ch = buf[count++]) != '\0')
        declared_func->reg[rc++] = ch;

    declared_func->no_of_params = rc; /* record no. of parameters */
    if (debug_msg)
        fprintf(stderr, "%s has offset %d Params: %d\n", func, offset, rc);
    if (ch != '\0')
        return (FALSE); /* last character should be NULL */

    /* we found it -> return */
    return (TRUE);

}

/* functions  misc.c */

void push_num_constant(int typ, SYM *item) {
    /* push a numeric
     constant onto
     the stack.
     */
    char buf[40], numbuf[40];

    strcpy(numbuf, "#\0");
    switch (typ) {
        case shorttype:
            itoa(item->numconst.shortnum, buf, 10L);
            break;
        case longtype:
            ltoa(item->numconst.longnum, buf, 10);
            break;
        case singletype:
            /*  sprintf (buf, "%lx", item->numconst.singlenum); original */
            sprintf(buf, "%x", item->numconst.singlenum);
            strcat(numbuf, "$");
            break;
    }

    strcat(numbuf, buf);

    if (typ == shorttype)
        gen("move.w", numbuf, "-(sp)");
    else
        gen("move.l", numbuf, "-(sp)");
}

/* functions  parse.c */

void compile(char *source_name, char *dest_name) {
    char buf[40], bytes[40], icon_name[MAXSTRLEN];
    FILE *icon_src, *icon_dest;
    int cc;

    /*
     ** Parse the source file producing XREFs, code, data,
     ** bss & basdata segments.
     */
    parse();

    /* optimise? */
    if (optimise_opt && !early_exit)
        optimise();

    if (!module_opt) {
        /* startup xrefs for startup.lib */
        enter_XREF("_startup");
        enter_XREF("_cleanup");

        /* command line argument xref */
        if (cli_args)
            enter_XREF("_parse_cli_args");

        if (translateused) {
            enter_XREF("_opentranslator");
            enter_XREF("_closetranslator");
        }

        if (mathffpused) {
            enter_XREF("_openmathffp");
            enter_XREF("_closemathffp");
        }

        if (mathtransused) {
            enter_XREF("_openmathtrans");
            enter_XREF("_closemathtrans");
        }

        if (gfxused) {
            enter_XREF("_opengfx");
            enter_XREF("_closegfx");
            enter_XREF("_openintuition");
            enter_XREF("_closeintuition");
        }

        if (intuitionused) {
            enter_XREF("_openintuition");
            enter_XREF("_closeintuition");
        }

        if (iffused) {
            enter_XREF("_create_ILBMLib");
            enter_XREF("_remove_ILBMLib");
        }

        enter_XREF("_starterr");

        /*
         ** A module may need to jump to _EXIT_PROG so
         ** make this label externally referenceable (* = XDEF).
         */
        enter_XREF("*EXIT_PROG");

        if (ontimerused)
            enter_XREF("_ontimerstart");

        /*
         ** Always call this in case a db.lib function
         ** allocates memory via alloc(). This also takes
         ** care of the use of ALLOC by an ACE program.
         ** To do this we always need to externally
         ** reference the free_alloc() function.
         */
        enter_XREF("_free_alloc");
    } else {
        /*
         ** Current module may need to jump to _EXIT_PROG, so externally reference it.
         */
        enter_XREF("_EXIT_PROG");
    }

    /* DATA statements? */
    if (basdatapresent)
        enter_BSS("_dataptr:", "ds.l 1");
    if ((readpresent) && (!basdatapresent))
        _error(25);

    /* ------------------------------------------------- */
    /* create A68K compatible 68000 assembly source file */
    /* ------------------------------------------------- */

    if (!early_exit)
        printf("\ncreating %s\n", dest_name);
    else
        printf("\nfreeing code list...\n");

    if (!early_exit)
        write_xrefs();

    /* startup code */
    fprintf(dest, "\n\tSECTION code,CODE\n\n");

    if (!module_opt) {
        /*
         ** Check for Wb start BEFORE DOING ANYTHING ELSE!
         ** This also always opens dos.library and stores
         ** CLI argument data.
         */
        fprintf(dest, "\tjsr\t_startup\n");
        fprintf(dest, "\tcmpi.b\t#1,_starterr\n"); /* see _startup in startup.lib */
        fprintf(dest, "\tbne.s\t_START_PROG\n");
        fprintf(dest, "\trts\n");
        fprintf(dest, "_START_PROG:\n");

        /* storage for initial stack pointer */
        enter_BSS("_initialSP:", "ds.l 1");
        fprintf(dest, "\tmove.l\tsp,_initialSP\n"); /* save task's stack pointer */

        fprintf(dest, "\tmovem.l\td1-d7/a0-a6,-(sp)\n"); /* save initial registers */

        if (cli_args)
            fprintf(dest, "\tjsr\t_parse_cli_args\n"); /* get CLI arguments */

        if (translateused) {
            fprintf(dest, "\tjsr\t_opentranslator\n");
            fprintf(dest, "\tcmpi.b\t#1,_starterr\n");
            fprintf(dest, "\tbne.s\t_translate_ok\n");
            fprintf(dest, "\tjmp\t_ABORT_PROG\n");
            fprintf(dest, "_translate_ok:\n");
        }

        if (mathffpused) {
            fprintf(dest, "\tjsr\t_openmathffp\n");
            fprintf(dest, "\tcmpi.b\t#1,_starterr\n");
            fprintf(dest, "\tbne.s\t_mathffp_ok\n");
            fprintf(dest, "\tjmp\t_ABORT_PROG\n");
            fprintf(dest, "_mathffp_ok:\n");
        }

        if (mathtransused) {
            fprintf(dest, "\tjsr\t_openmathtrans\n");
            fprintf(dest, "\tcmpi.b\t#1,_starterr\n");
            fprintf(dest, "\tbne.s\t_mathtrans_ok\n");
            fprintf(dest, "\tjmp\t_ABORT_PROG\n");
            fprintf(dest, "_mathtrans_ok:\n");
        }

        if (intuitionused && !gfxused) {
            fprintf(dest, "\tjsr\t_openintuition\n");
            fprintf(dest, "\tcmpi.b\t#1,_starterr\n");
            fprintf(dest, "\tbne.s\t_intuition_ok\n");
            fprintf(dest, "\tjmp\t_ABORT_PROG\n");
            fprintf(dest, "_intuition_ok:\n");
        }

        if (gfxused) {
            /* open intuition.library */
            fprintf(dest, "\tjsr\t_openintuition\n");
            fprintf(dest, "\tcmpi.b\t#1,_starterr\n");
            fprintf(dest, "\tbne.s\t_intuition_ok\n");
            fprintf(dest, "\tjmp\t_ABORT_PROG\n");
            fprintf(dest, "_intuition_ok:\n");

            /* open graphics.library */
            fprintf(dest, "\tjsr\t_opengfx\n");
            fprintf(dest, "\tcmpi.b\t#1,_starterr\n");
            fprintf(dest, "\tbne.s\t_gfx_ok\n");
            fprintf(dest, "\tjmp\t_ABORT_PROG\n");
            fprintf(dest, "_gfx_ok:\n");
        }

        /* create temporary ILBM.library */
        if (iffused)
            fprintf(dest, "\tjsr\t_create_ILBMLib\n");

        /* get timer event trapping start time */
        if (ontimerused)
            fprintf(dest, "\tjsr\t_ontimerstart\n");

        /* size of stack frame */
        if (addr[lev] == 0)
            strcpy(bytes, "#\0");
        else
            strcpy(bytes, "#-");
        itoa(addr[lev], buf, 10L);
        strcat(bytes, buf);

        /* create stack frame */
        fprintf(dest, "\tlink\ta4,%s\n\n", bytes);

        /* initialise global DATA pointer */
        if (basdatapresent)
            fprintf(dest, "\tmove.l\t#_BASICdata,_dataptr\n");
    }

    /* write code & kill code list */
    kill_code();

    if (!module_opt) {
        /* exiting code */
        fprintf(dest, "\n_EXIT_PROG:\n");

        fprintf(dest, "\tunlk\ta4\n");

        /*
         ** Programs which abort should cleanup libraries, free allocated memory
         ** and possibly reply to a Wb startup message.
         */
        if (intuitionused || gfxused || mathffpused || mathtransused
            || translateused)
            fprintf(dest, "_ABORT_PROG:\n");

        /* Free memory allocated via ALLOC and db.lib calls to alloc(). */
        fprintf(dest, "\tjsr\t_free_alloc\n");

        /* close libraries */
        if (gfxused) {
            fprintf(dest, "\tjsr\t_closegfx\n");
            fprintf(dest, "\tjsr\t_closeintuition\n");
        }
        if (narratorused)
            fprintf(dest, "\tjsr\t_cleanup_async_speech\n");
        if (intuitionused && !gfxused)
            fprintf(dest, "\tjsr\t_closeintuition\n");
        if (mathtransused)
            fprintf(dest, "\tjsr\t_closemathtrans\n");
        if (mathffpused)
            fprintf(dest, "\tjsr\t_closemathffp\n");
        if (translateused)
            fprintf(dest, "\tjsr\t_closetranslator\n");

        /* delete temporary ILBM.library */
        if (iffused)
            fprintf(dest, "\tjsr\t_remove_ILBMLib\n");

        /* restore registers */
        fprintf(dest, "\tmovem.l\t(sp)+,d1-d7/a0-a6\n");

        /* restore initial stack pointer */
        fprintf(dest, "\tmove.l\t_initialSP,sp\n");

        /*
         ** Close dos.library and reply to Wb message
         ** as the LAST THING DONE before rts'ing.
         */
        fprintf(dest, "\tjsr\t_cleanup\n");

        /* return */
        fprintf(dest, "\n\trts\n");
    }

    if (!early_exit) {
        write_data();
        write_basdata();
        write_bss();
    }

    fprintf(dest, "\n\tEND\n");

    /* errors? */
    if (errors > 0)
        putchar('\n');

    printf("%s compiled ", source_name);

    if (errors == 0)
        printf("with no errors.\n");
    else {
        exitvalue = 10; /* set ERROR for bas script */
        printf("with %d ", errors);
        if (errors > 1)
            printf("errors.\n");
        else
            printf("error.\n");
    }

    /* make icon? */
    if (make_icon && !early_exit) {
        char ipath[256];
        char *b = getenv("ACE_Basic");
        if (b) {
            sprintf(ipath, "%sicons/exe.info", b);
        }
        if (b && (icon_src = fopen(ipath, "r")) == NULL)
            fprintf(stderr, "error: can't open \"%s\" for reading.\n", ipath);
        else {
            cc = 0;
            while (source_name[cc] != '.')
                cc++;
            source_name[cc] = '\0';
            sprintf(icon_name, "%s.info", source_name);
            if ((icon_dest = fopen(icon_name, "w")) == NULL)
                printf("can't open %s.info for writing.\n", source_name);
            else {
                while (!feof(icon_src))
                    fputc(fgetc(icon_src), icon_dest);
                fclose(icon_dest);
                fclose(icon_src);
                puts("icon created.");
            }
        }
    }
}

void block(void) {
    CODE *link;
    SYM *sub_ptr;
    char end_of_sub_name[80], end_of_sub_label[80];
    char sub_name[80], sub_label[80], exit_sub_label[80];
    char xdef_name[80];
    char bytes[40], buf[40];
    int subprog;
    int sub_type, def_expr_type;

    while (!end_of_source) {
        if (sym != subsym && sym != defsym)
            /* ordinary statement */
            statement();
        else {
            /************************/
            /* SUBprogram or DEF FN */
            /************************/
            subprog = sym;
            insymbol();

            sub_type = undefined;

            /* type identifiers */
            if (sym == shortintsym || sym == longintsym || sym == addresssym
                || sym == singlesym || sym == stringsym) {
                switch (sym) {
                    case shortintsym:
                        sub_type = shorttype;
                        break;
                    case longintsym:
                        sub_type = longtype;
                        break;
                    case addresssym:
                        sub_type = longtype;
                        break;
                    case singlesym:
                        sub_type = singletype;
                        break;
                    case stringsym:
                        sub_type = stringtype;
                        break;
                }
                insymbol();
            }

            if (sym != ident)
                _error(32);
            else {
                /* get name of subprogram and prefix _SUB_ to it */
                strcpy(sub_name, "_SUB_");
                strcat(sub_name, id);

                if (!exist(sub_name, subprogram)) {
                    if (sub_type == undefined)
                        sub_type = typ;
                    enter(sub_name, sub_type, subprogram, 0); /* new SUB */
                    curr_item->decl = subdecl;
                } else if ((exist(sub_name, subprogram))
                           && (curr_item->decl == fwdref))
                    curr_item->decl = subdecl;
                else
                    _error(33); /* already exists */

                sub_ptr = curr_item; /* pointer to sub info' */

                turn_event_off(sub_name); /* see event.c */

                /* exit point name & label */
                strcpy(exit_sub_name, "_EXIT");
                strcat(exit_sub_name, sub_name);
                strcpy(exit_sub_label, exit_sub_name);
                strcat(exit_sub_label, ":\0");

                /* prepare for level ONE */
                lev = ONE;
                addr[lev] = 0;
                new_symtab();
                make_label(end_of_sub_name, end_of_sub_label);
                gen("jmp", end_of_sub_name, "  ");

                /* subprogram label -> _SUB_ prefix to make it unique */
                strcpy(sub_label, sub_name);
                strcat(sub_label, ":\0");
                gen(sub_label, "  ", "  ");

                /* all SUBs need link instruction -- add # of bytes later */
                gen("link", "a5", "  ");
                link = curr_code;

                /* parse formal parameter list */
                sub_params(sub_ptr);

                /* make this subprogram externally visible? */
                if (sym == externalsym) {
                    insymbol();
                    strcpy(xdef_name, sub_name);
                    xdef_name[0] = '*'; /* signal that this is an XDEF */
                    enter_XREF(xdef_name);
                }

                /*
                 ** Pass function (SUB) values via d0 for ALL subprograms
                 ** in a module since there is no link using A4 for modules.
                 */
                if (module_opt)
                    sub_ptr->address = extfunc; /* This has a numeric value of 3004:
					 hopefully large enough to avoid
					 clashes with real stack offsets. */

                /* SUB or DEF FN code? */
                if (subprog == subsym) {
                    while ((sym != endsym) && (!end_of_source)) {
                        if (sym == sharedsym)
                            parse_shared_vars();
                        if ((sym != endsym) && (!end_of_source))
                            statement();
                    }

                    if (end_of_source)
                        _error(34); /* END SUB expected */

                    if (sym == endsym) {
                        insymbol();
                        if (sym != subsym)
                            _error(35);
                        insymbol();
                    }
                } else {
                    /* DEF FN code */
                    if (sym != equal)
                        _error(5);
                    else {
                        insymbol();
                        def_expr_type = expr();
                        if (assign_coerce(sub_type, def_expr_type) != sub_type)
                            _error(4);
                        else if (sub_type == shorttype)
                            gen("move.w", "(sp)+", "d0");
                        else
                            gen("move.l", "(sp)+", "d0");

                        /* change object from SUB to DEF FN */
                        sub_ptr->object = definedfunc;
                    }
                }

                /* establish size of stack frame */
                if (addr[lev] == 0)
                    strcpy(bytes, "#\0");
                else
                    strcpy(bytes, "#-");
                itoa(addr[lev], buf, 10L);
                strcat(bytes, buf);
                change(link, "link", "a5", bytes);

                /* exit code */
                if (subprog == subsym)
                    gen(exit_sub_label, "  ", "  ");
                gen("unlk", "a5", "  ");
                gen("rts", "  ", "  ");
                gen(end_of_sub_label, "  ", "  ");

                kill_symtab();
                lev = ZERO;
            }
        }
    }
}

/* functions  sub.c */
void load_params(SYM *sub_ptr) {
    long par_addr = -8; /* one word above stack frame
	 (allows for R.A. & address reg store) */
    SHORT i, n;
    int formal_type;
    char addrbuf[40];
    char formaltemp[MAXPARAMS][80], formaladdr[MAXPARAMS][80];
    int formaltype[MAXPARAMS];

    /* store actual parameters in stack frame of subprogram to be CALLed */

    if (sym != lparen) {
        _error(14);
        return;
    } else {
        i = 0;
        do {
            insymbol();
            formal_type = expr();

            /* check parameter types */
            if (formal_type != sub_ptr->p_type[i]) {
                /* coerce actual parameter type to formal parameter type */
                switch (sub_ptr->p_type[i]) {
                    case shorttype:
                        make_sure_short(formal_type);
                        break;

                    case longtype:
                        if ((formal_type = make_integer(formal_type)) == shorttype)
                            make_long();
                        else if (formal_type == notype)
                            _error(4); /* string */
                        break;

                    case singletype:
                        gen_Flt(formal_type);
                        break;

                    case stringtype:
                        _error(4); /* can't coerce this at all! */
                        break;
                }
            }

            /* store parameter information temporarily since further stack operations
             may corrupt data in next frame if stored immediately */
            if (sub_ptr->p_type[i] == shorttype) {
                par_addr -= 2;
                /* save parameter type */
                formaltype[i] = shorttype; /* not data TYPE but STORE type (2 or 4 bytes) */

                /* save address of formal */
                itoa(par_addr, addrbuf, 10);
                strcat(addrbuf, "(sp)");
                strcpy(formaladdr[i], addrbuf);

                /* create temporary store in current stack frame -> don't use a global
                 data object as it could be clobbered during recursion! */
                addr[lev] += 2;
                itoa(-1 * addr[lev], formaltemp[i], 10L);
                strcat(formaltemp[i], frame_ptr[lev]);

                /* store it */
                gen("move.w", "(sp)+", formaltemp[i]);
            } else
                /* long, single, string, array */
            {
                par_addr -= 4;
                /* save parameter type */
                formaltype[i] = longtype; /* storage requirement is 4 bytes */

                /* save address of formal */
                itoa(par_addr, addrbuf, 10);
                strcat(addrbuf, "(sp)");
                strcpy(formaladdr[i], addrbuf);

                /* create temporary store in current stack frame -> don't use a global
                 data object as it could be clobbered during recursion! */
                addr[lev] += 4;
                itoa(-1 * addr[lev], formaltemp[i], 10L);
                strcat(formaltemp[i], frame_ptr[lev]);

                /* store it */
                gen("move.l", "(sp)+", formaltemp[i]);
            }

            i++;
        } while ((i < sub_ptr->no_of_params) && (sym == comma));

        if ((i < sub_ptr->no_of_params) || (sym == comma))
            _error(39); /* parameter count mismatch - too few or too many resp. */
        else {
            /* disable multi-tasking
             before passing parameters */
            gen("movea.l", "_AbsExecBase", "a6");
            gen("jsr", "_LVOForbid(a6)", "  ");
            enter_XREF("_AbsExecBase");
            enter_XREF("_LVOForbid");

            /* load parameters into next frame */
            for (n = 0; n < sub_ptr->no_of_params; n++) {
                if (formaltype[n] == shorttype)
                    gen("move.w", formaltemp[n], formaladdr[n]); /* short */
                else
                    gen("move.l", formaltemp[n], formaladdr[n]); /* long,string,single,array */
            }
        }

        if (sym != rparen)
            _error(9);
    }
}
