#define	MAXIDSIZE     42           /* max length of identifier (+ qualifier) */
#define	MAXPARAMS     40    	   /* max # of subprogram parameters */
#define	NUMACELIBS     7	   /* # of shared libraries used by ACE */
#define	NUMLIBS	      40	   /* max # of "other" shared libraries */

/* --symbol table-- */
typedef union conststruct {
			   SHORT shortnum;	
			   LONG  longnum;
			   float singlenum;
			  } CONST;

typedef struct structmem {
			  char  name[MAXIDSIZE];
			  int   type;
			  ULONG offset;
			  ULONG strsize;
			  struct structmem *next;
			 } STRUCM;
  
typedef struct symstruct {
			  char   *name;              /* name of identifier */
			  int    type;               /* type (short,long...) */
			  int    object;             /* variable,sub... */
			  int    dims;               /* # of array elements */
			  BOOL   shared;	     /* is object shared? */
			  BOOL   new_string_var;     /* new string variable? */
			  SHORT  *index;     	     /* array indices */
			  SHORT	 no_of_params;       /* # of SUB parameters */
			  int	 p_type[MAXPARAMS];  /* SUB parameter types */
			  UBYTE  decl;		     /* forward reference? */
			  UBYTE  *reg;	     	     /* lib function regs */
			  char   *libname;	     /* library name */
			  CONST  numconst;	     /* a numeric constant */
			  STRUCM *structmem;         /* structdef list ptr */
			  long    address;            /* frame/library offset */
			  ULONG  size;		     /* # of bytes in object */
			  int    level;              /* main or sub program */
			  struct symstruct *other;   /* for cross-reference */
			  struct symstruct *next;    /* next table entry */
		    	 } SYM;


/* --data list-- */
typedef struct datalist {
			 char *name;	        /* name of storage */
			 char *literal;         /* data */
			 struct datalist *next;
			} DATA;

/* --bss list-- */
typedef struct bsslist {
			 char *name;	        /* name of storage */
			 char *store;   	/* storage type & size */
			 struct bsslist *next;
		       } BSS;

/* --XREF list-- */
typedef struct xreflist {
			 char *name;           /* name of external reference */
			 struct xreflist *next;
			} XREF;

/* --BASIC DATA list-- */
typedef struct basicdatalist {
			 char *literal; /* string or float constant */
			 struct basicdatalist *next;
			} BASDATA;

/* --ACE library usage information-- */
typedef struct ACELIBS {
			   char name[MAXIDSIZE];   /* library name */
			   char base[MAXIDSIZE+5]; /* library base: _NAMEBase */
			  };

         struct ACELIBS acelib[NUMACELIBS];
         struct ACELIBS otherlib[NUMLIBS];


/* --code list-- */
typedef struct codelist {
			 char *opcode;          /* 68000 opcode */ 
			 char *srcopr;          /* source operand */
			 char *destopr;         /* destination operand */
			 struct codelist *next;
			} CODE;

