/* << ACE >> 

 -- Amiga BASIC Compiler --

 ** #includes, #defines and function prototypes ** 
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
 Date: 19th October-30th November, 3rd-12th December 1991,
 14th,27th January 1992, 5th,7th-17th, 
 23rd-24th February 1992,
 14th,22nd March 1992,
 21st April 1992,
 2nd,11th,15th May 1992,
 7th,8th,9th,11th,13th,14th,28th June 1992,
 2nd-6th,8th,14th-19th,26th,28th,29th July 1992,
 1st,9th August 1992,
 6th,28th December 1992,
 7th January 1993,
 11th,13th February 1993,
 1st,6th March 1993,
 12th April 1993,
 17th May 1993,
 12th,14th June 1993,
 1st July 1993,
 5th,25th September 1993,
 26th October 1993,
 6th,15th November 1993,
 17th,18th,24th December 1993,
 2nd,3rd,7th,9th January 1994,
 6th,15th,16th,27th February 1994,
 10th,14th July 1994,
 11th September 1994,
 13th May 1996,
 4th,22nd June 1996,
 4th September 1996
 */

//#include <exec/types.h>
#include <assert.h>
//#include <exec/memory.h>
//#include <libraries/mathffp.h>
//#include <libraries/mathlibrary.h>
//#include <libraries/dos.h>
#include <stdio.h>

#ifdef __GNUC__
/* Force compiling with builtin math for gcc */
#define SPFloor( a )      ( floor( a ) )
#define SPCmp( a, b )     ( ( a > b ) ? 1 : ( a < b ) ? -1 : 0 )
#define SPFix( a )        ( (int) a )
#define SPCeil( a )       ( ceil( a ) )
#define SPFlt( a )        ( (float) a )
#define SPSub( a, b )     ( (float)(a) - (float)(b) )
#define SPAdd( a, b )     ( (float)(a) + (float)(b) )
#define SPMul( a, b )     ( (float)(a) * (float)(b) )
#define SPDiv( a, b )     ( (float)(a) / (float)(b) )
#define SPPow( a, b )     ( pow( (float)(a) , (float)(b) ) )

#ifndef TRUE
#define TRUE            1
#endif
#ifndef FALSE
#define FALSE           0
#endif
#ifndef NULL
#define NULL            0L
#endif

#define MEMF_ANY	0
#define SIGBREAKF_CTRL_C 0

typedef short SHORT;
typedef unsigned long ULONG;
typedef long LONG;
typedef short BOOL;
typedef unsigned char UBYTE;
typedef char BYTE;
typedef void *APTR;
typedef unsigned char *STRPTR; /* string pointer (NULL terminated) */

#endif

/* AmigaBASIC reserved words */
enum {
	abssym = 0,
	allsym,
	andsym,
	appendsym,
	areasym,
	areafillsym,
	assym,
	ascsym,
	atnsym,
	basesym,
	beepsym,
	breaksym,
	callsym,
	cdblsym,
	chainsym,
	chdirsym,
	chrstrsym,
	cintsym,
	circlesym,
	clearsym,
	clngsym,
	closesym,
	clssym,
	collisionsym,
	colorsym,
	commonsym,
	contsym,
	cossym,
	csngsym,
	csrlinsym,
	cvdsym,
	cvisym,
	cvlsym,
	cvssym,
	datasym,
	datestrsym,
	declaresym,
	defsym,
	defdblsym,
	defintsym,
	deflngsym,
	defsngsym,
	defstrsym,
	deletesym,
	dimsym,
	elsesym,
	elseifsym,
	endsym,
	eofsym,
	eqvsym,
	erasesym,
	erlsym,
	errsym,
	errorsym,
	exitsym,
	expsym,
	fieldsym,
	filessym,
	fixsym,
	fnsym,
	forsym,
	fresym,
	functionsym,
	getsym,
	gosubsym,
	gotosym,
	hexsym,
	ifsym,
	impsym,
	inkeysym,
	inputsym,
	inputstrsym,
	instrsym,
	intsym,
	killsym,
	lboundsym,
	leftstrsym,
	lensym,
	letsym,
	librarysym,
	linesym,
	listsym,
	llistsym,
	loadsym,
	locsym,
	locatesym,
	lofsym,
	logsym,
	lpossym,
	lprintsym,
	lsetsym,
	menusym,
	mergesym,
	midstrsym,
	mkdstrsym,
	mkistrsym,
	mklstrsym,
	mksstrsym,
	modsym,
	mousesym,
	namesym,
	newsym,
	nextsym,
	notsym,
	objaxsym,
	objaysym,
	objclipsym,
	objclosesym,
	objhitsym,
	objoffsym,
	objonsym,
	objplanessym,
	objprioritysym,
	objshapesym,
	objstartsym,
	objstopsym,
	objvxsym,
	objvysym,
	objxsym,
	objysym,
	octstrsym,
	offsym,
	onsym,
	opensym,
	optionsym,
	orsym,
	outputsym,
	paintsym,
	palettesym,
	patternsym,
	peeksym,
	peeklsym,
	peekwsym,
	pointsym,
	pokesym,
	pokelsym,
	pokewsym,
	possym,
	presetsym,
	printsym,
	psetsym,
	ptabsym,
	putsym,
	randomizesym,
	readsym,
	remsym,
	restoresym,
	resumesym,
	returnsym,
	rightstrsym,
	rndsym,
	rsetsym,
	runsym,
	saddsym,
	savesym,
	saysym,
	screensym,
	scrollsym,
	sgnsym,
	sharedsym,
	sinsym,
	sleepsym,
	soundsym,
	spacestrsym,
	spcsym,
	sqrsym,
	staticsym,
	stepsym,
	sticksym,
	stopsym,
	strstrsym,
	strigsym,
	stringstrsym,
	subsym,
	swapsym,
	systemsym,
	tabsym,
	tansym,
	thensym,
	timestrsym,
	timersym,
	tosym,
	translatestrsym,
	troff,
	tron,
	uboundsym,
	ucasestrsym,
	usingsym,
	valsym,
	varptrsym,
	waitsym,
	wavesym,
	wendsym,
	whilesym,
	widthsym,
	windowsym,
	writesym,
	xorsym,

	/* ACE-specific reserved words */
	addresssym,
	allocsym,
	argstrsym,
	argcountsym,
	assemsym,
	backsym,
	bevelboxsym,
	binstrsym,
	blocksym,
	buttonsym,
	bytesym,
	casesym,
	constsym,
	cstrsym,
	daysym,
	externalsym,
	fileboxstrsym,
	fontsym,
	forwardsym,
	gadgetsym,
	globalsym,
	handlesym,
	headingsym,
	homesym,
	iffsym,
	inputboxsym,
	inputboxstrsym,
	longintsym,
	messagesym,
	msgboxsym,
	pendownsym,
	penupsym,
	potxsym,
	potysym,
	printssym,
	repeatsym,
	serialsym,
	setheadingsym,
	setxysym,
	shlsym,
	shortintsym,
	shrsym,
	singlesym,
	sizesym,
	sizeofsym,
	stringsym,
	structsym,
	stylesym,
	turnsym,
	turnleftsym,
	turnrightsym,
	turtlesym,
	untilsym,
	xcorsym,
	ycorsym,

	RWSENTINEL
};

/* special symbols */
enum {
	increment = 500,
	decrement,
	shortpointer,
	longpointer,
	singlepointer,
	memberpointer,
	plus,
	minus,
	multiply,
	fdiv,
	idiv,
	raiseto,
	lparen,
	rparen,
	comma,
	apostrophe,
	semicolon,
	becomes,
	colon,
	question,
	notequal,
	ltorequal,
	gtorequal,
	equal,
	lessthan,
	gtrthan,
	hash,
	octalprefix,
	hexprefix,
	atsymbol,
	endofline,

	RSSENTINEL
};

/* symbols */
enum {
	shortconst = 1000, longconst, singleconst, doubleconst, stringconst, ident
};

/* types */
enum {
	bytetype = 2000,
	shorttype,
	longtype,
	singletype,
	doubletype,
	stringtype,
	notype
};

/* objects */
enum {
	variable = 3000,
	array,
	subprogram,
	definedfunc,
	function,
	extfunc,
	extvar,
	label,
	constant,
	structdef,
	structure,
	rsvd_word,

	undefined = 9999
};
#define NDEBUG            /* to strip asserts code without removing asserts */
#define	MAXLINELEN   1024	/* max length of a BASIC line */
#define	MAXIDSIZE     42	/* max length of identifier (+ qualifier) */
#define	MAXSTRLEN   1024	/* max length of string */
#define	MAXSHORT   32767	/* max +ve short value */
#define	MINSHORT  -32768	/* min -ve short value */
#define	MAXLONG    2147483647	/* max +ve long value */
#define	MINLONG   -2147483648	/* min -ve long value */
#define	MAXPARAMS     40	/* max # of subprogram parameters */
#define	NEGATIVE      -1	/* a negative result from ACE lib check */
#define	MAXCASES     1000	/* max # of cases in a CASE statement */
#define	NUMACELIBS     7	/* # of shared libraries used by ACE */
#define	NUMLIBS	      40	/* max # of "other" shared libraries */
#define	LF_CODE	       0	/* line feed code for PRINTS */
#define	TAB_CODE       1	/* tab code for PRINTS */
#define	SPACE_CODE     2	/* single space code for PRINTS */

/* --symbol table-- */
#define	ZERO        0
#define	ONE         1
#define	fwdref      0
#define	subdecl     1
#define	undeclared  2		/* undeclared string variable (eg: x$="A") */
#define	declared    3		/* string variable declared with STRING */
#define	MAXDIMS   255

/* --symbol table-- */
typedef union conststruct {
	SHORT shortnum;
	LONG longnum;
	float singlenum;
} CONST;

typedef struct structmem {
	char name[MAXIDSIZE];
	int type;
	ULONG offset;
	ULONG strsize;
	struct structmem *next;
} STRUCM;

typedef struct symstruct {
	char *name; /* name of identifier */
	int type; /* type (short,long...) */
	int object; /* variable,sub... */
	int dims; /* # of array elements */
	BOOL shared; /* is object shared? */
	BOOL new_string_var; /* new string variable? */
	SHORT *index; /* array indices */
	SHORT no_of_params; /* # of SUB parameters */
	int p_type[MAXPARAMS]; /* SUB parameter types */
	UBYTE decl; /* forward reference? */
	UBYTE *reg; /* lib function regs */
	char *libname; /* library name */
	CONST numconst; /* a numeric constant */
	STRUCM *structmem; /* structdef list ptr */
	long address; /* frame/library offset */
	ULONG size; /* # of bytes in object */
	int level; /* main or sub program */
	struct symstruct *other; /* for cross-reference */
	struct symstruct *next; /* next table entry */
} SYM;

/* --code generator-- */

/* --code list-- */
typedef struct codelist {
	char *opcode; /* 68000 opcode */
	char *srcopr; /* source operand */
	char *destopr; /* destination operand */
	struct codelist *next;
} CODE;

/* --data list-- */
typedef struct datalist {
	char *name; /* name of storage */
	char *literal; /* data */
	struct datalist *next;
} DATA;

/* --bss list-- */
typedef struct bsslist {
	char *name; /* name of storage */
	char *store; /* storage type & size */
	struct bsslist *next;
} BSS;

/* --XREF list-- */
typedef struct xreflist {
	char *name; /* name of external reference */
	struct xreflist *next;
} XREF;

/* --BASIC DATA list-- */
typedef struct basicdatalist {
	char *literal; /* string or float constant */
	struct basicdatalist *next;
} BASDATA;

/* --ACE library usage information--  */
typedef struct aceliblist {
	char name[MAXIDSIZE]; /* library name */
	char base[MAXIDSIZE + 5]; /* library base: _NAMEBase */
} ACELIBS;

/*
 struct aceliblist
 {
 char name[MAXIDSIZE];	/* library name 
 char base[MAXIDSIZE + 5];	/* library base: _NAMEBase 
 };

 this is the new way.
 #define ACELIBS struct aceliblist */

/* --function protos-- */

/* opt.c */
BOOL is_a_move(char *opcode);
SHORT peephole(void);
void optimise(void);

/* lex.c */
void open_shared_libs(void);
void close_shared_libs(void);
void setup(void);
void cleanup(void);
void _warn(int n);
void _error(int n);
void _abort(int n);
void open_files(char *source);
void nextch(void);
BOOL letter(void);
BOOL digit(void);
int hex_digit(void);
int octal_digit(void);
void convert_special_ident(void);
BOOL qualifier(void);
BOOL ssymbol(void);
int rsvd_wd(char *id);
int rsvd_sym(char *id);
void reclassify_number(void);
void classify_integer(LONG n);
void insymbol(void);
void showsym(int sym);
void showobj(int typ);
void showtyp(int typ);
void tab(void);
void lf(void);

/* alloc.c */
void *alloc(ULONG bytes, ULONG flags);
void *sym_alloc(ULONG bytes, ULONG flags);
void free_alloc(void);
void free_sym_alloc(void);
CODE *alloc_code(char *opcode, char *srcopr, char *destopr);
void free_code(CODE * cnode);
BOOL alloc_code_members(CODE * cnode, char *opcode, char *srcopr, char *destopr);
void free_code_members(CODE * cnode);

/* sym.c */
void new_symtab(void);
void kill_symtab(void);
void find_tab_tail(void);
BOOL exist(char *name, int obj);
void enter(char *name, int typ, int obj, int dims);
void create_lists(void);
BOOL is_a_label(char *opc);
void write_code(CODE * line);
BOOL label_undef(CODE * node);
void undef_label_check(void);
void kill_code(void);
void gen(char *opcode, char *srcopr, char *destopr);
void change(CODE * cx, char *opcode, char *srcopr, char *destopr);
BOOL exist_DATA(char *name);
void enter_DATA(char *name, char *literal);
void write_data(void);
BOOL exist_BSS(char *name);
void enter_BSS(char *name, char *store);
void write_bss(void);
BOOL exist_XREF(char *name);
void enter_XREF(char *name);
void write_xrefs(void);
void enter_BASDATA(char *literal);
void write_basdata(void);
void find_structmem_tail(SYM * symtabitem);
BOOL structmem_exist(SYM * symtabitem, char *name);
void add_struct_member(SYM * symtabitem, char *name, int mtype,
		SYM * structtype);
void kill_all_lists(void);

/* basfun.c */
BOOL strfunc(void);
int stringfunction(void);
int gen_single_func(char *funcname, int nftype);
BOOL numfunc(void);
int numericfunction(void);
int address_of_object(void);
int find_object_size(void);

/* functions in main ace modules */
/* misc.c */
void make_temp_long(void);
void make_temp_short(void);
void make_temp_string(void);
void make_string_const(char *string);
void make_label_from_linenum(int intconst, char *buf);
LONG max_array_ndx(SYM * curr);
void push_indices(SYM * curr);
void get_abs_ndx(SYM * curr);
void push_num_constant(int typ, SYM * item);
int push_struct(SYM * item);
void change_id_type(int newtype);
void gen_branch(char *branch, char *labname);
void assem(void);
void parse_option_list(void);
void MsgBox(void);

/* factor.c */
BOOL factorfunc(void);
int factor(void);

/* expr.c */
BOOL coerce(int *typ1, int *typ2, CODE * cx[]);
void make_short(void);
void make_long(void);
int ptr_term(void);
int expterm(void);
int negterm(void);
int prodterm(void);
int idivterm(void);
int modterm(void);
int simple_expr(void);
BOOL relop(int op);
char *cond_branch_op(int op);
void make_label(char *name, char *lab);
int relexpr(void);
int notexpr(void);
int andexpr(void);
int orexpr(void);
int eqvexpr(void);
int expr(void);
void pop_operands(int typ);
void push_result(int typ);
void gen_round(int type);
void gen_Flt(int typ);
void change_Flt(int exptyp, CODE * cx[]);
int make_integer(int oldtyp);
void make_sure_short(int type);
void make_sure_long(int type);

/* assign.c */
int assign_coerce(int storetype, int exptype);
void make_string_store(void);
void create_string_variable(SYM * string_item, long string_size);
void assign_to_string_variable(SYM * string_item, long string_size);
void assign_to_string_array(char *addrbuf);
void assign_to_struct(SYM * item);
void assign(void);
void make_array_name(char *name, char *lab);
void dim(void);
void input(void);
void point_to_array(SYM * storage, char *addrbuf);
void make_data_const(char *string);
void get_data(void);
void read_data(void);

/* print.c */
void gen_printcode(int code);
void print_statement(void);
void gen_printscode(int code);
void prints_statement(void);

/* control.c */
void block_if(CODE * cx1);
void if_statement(void);
void while_statement(void);
void repeat_statement(void);
void case_statement(void);
int for_assign(char *addr);
void for_statement(void);
void on_branch(void);
void block_statement(void);

/* gfx.c */
void pset(void);
void paint(void);
void circle(void);
void draw_line(void);
void color(void);
void area(void);
void areafill(void);
void pattern(void);
void scroll(void);
void text_style(void);
void text_font(void);
void gfx_get(void);
void gfx_put(void);

/* screen.c */
void screen(void);

/* window.c */
void window(void);
void wdwclose(void);
void wdwoutput(void);

/* gadget.c */
void gadget_rectangle(void);
void close_gadget(void);
void gadget_output(void);
void wait_gadget(void);
void modify_gadget(void);
void gadget(void);
void bevel_box(void);
void gadget(void);

/* menu.c */
void clear_menu(void);
void wait_menu(void);
void menu(void);

/* file.c */
void open_a_file(void);
void close_a_file(void);
void line_input(void);
void write_to_file(void);
void gen_writecode(int code);
void print_to_file(void);
void input_from_file(void);
void kill(void);
void ace_rename(void);
void ace_chdir(void);
void files(void);
void random_file_get(void);
void random_file_put(void);

/* serial.c */
void serial_command(void);
void open_serial(void);
void close_serial(void);
void read_serial(void);
void write_serial(void);

/* message.c */
void message_open(void);
void message_read(void);
void message_write(void);
void message_wait(void);
void message_clear(void);
void message_close(void);
void message(void);

/* iff.c */
void iff_open(void);
void iff_read(void);
void iff_close(void);
void iff(void);

/* libfunc.c */
BYTE check_for_ace_lib(char *libname);
void enter_new_library(char *libname);
void make_library_base(char *libname);
void make_library_name(char *libname);
void make_bmap_name(char *libname);
void get_libname(char *libname, char *ut_libname);
void library(void);
void closelibrary(void);
void remove_qualifier(char *funcname);
BOOL search_func(char *bmap, char *func, SYM * declared_func);
BOOL found_func(char *libname, char *ut_funcname, SYM * declared_func);
void declare(void);
void load_func_params(SYM * func_item);
void load_mc_params(SYM * sub_ptr);

/* statement.c */
void sound(void);
void handle_label(char *label_name);
void statement(void);

/* event.c */
void get_event_trap_label(void);
void change_event_trapping_status(int event);
void turn_event_off(char *eventHandler);
void check_for_event(void);
void ctrl_c_test(void);
void break_event_test(void);
void menu_event_test(void);
void mouse_event_test(void);
void timer_event_test(void);
void error_event_test(void);
void wdw_close_test(void);
void wdw_event_test(void);
void gad_event_test(void);

/* declare.c */
void define_structure(void);
void declare_structure(void);
void define_constant(void);
void declare_variable(int vartype);
void define_external_object(void);
void define_external_variable(void);
void define_external_function(void);
void call_external_function(char *extfuncid, BOOL * need_symbol);
void define_common_or_global_variable(int varsym);

/* memory.c */
void poke(void);
void pokew(void);
void pokel(void);
void get_obj_info(char *objname, int *object, int *objtype);
void swap(void);

/* sub.c */
void forward_ref(void);
void load_params(SYM * sub_ptr);
void sub_params(SYM * sub_ptr);
void parse_shared_vars(void);

/* parse.c */
void block(void);
void parse(void);
void compile(char *source, char *dest_name);
void show_title(void);
void usage(void);
BOOL check_options(char *opt);
void dump_reserved_words(void);
int main(int argc, char *argv[]);

/* extras.c */
char *ultoa(unsigned long n, char *buffer, int radix);
long fsize(char *name);
char *itoa(long n, char *buffer, int radix);
char *ltoa(long n, char *buffer, int radix);
char *strupr(char *string);
