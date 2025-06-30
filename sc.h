/*	SC	A Table Calculator
 *		Common definitions
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *		R. Bond  12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 7.16 $
 *
 */

#ifdef MSDOS
#include <stdio.h>
#endif

#if defined (__unix__) || (defined (__APPLE__) && defined (__MACH__))
#include <unistd.h>
#endif

#define CLEAR_LINE error("%s", "") /* suppress warning on NetBSD curses */
#define	ATBL(tbl, row, col)	(*(tbl + row) + (col))

#define MINROWS 100 	/* minimum size at startup */
#define MINCOLS 30 
#define	ABSMAXCOLS 702	/* absolute cols: ZZ (base 26) */

#define CRROWS 1
#define CRCOLS 2
#define RESROW 3 /* rows reserved for prompt, error, and column numbers */

/* formats for engformat() */
#define REFMTFIX	0
#define REFMTFLT	1
#define REFMTENG	2
#define REFMTDATE	3
#define REFMTLDATE	4

#define DEFWIDTH 10	/* Default column width and precision */
#define DEFPREC   2
#define DEFREFMT  REFMTFIX /* Make default format fixed point  THA 10/14/90 */

#define FKEYS		 24	/* Number of function keys available */
#define HISTLEN		100	/* Number of history entries for vi emulation */
#define CPAIRS		  8	/* Number of color pairs available */
#define COLFORMATS	 10	/* Number of custom column formats */
#define DELBUFSIZE	 40	/* Number of named buffers + 4 */
#ifdef PSC
# define error(msg)	fprintf(stderr, msg);
#else
# define error if (isatty(fileno(stdout)) && !move(1,0) && !clrtoeol()) printw
#endif
#ifndef FBUFLEN
#  define       FBUFLEN 1024    /* buffer size for a single field */
#endif
#define PATHLEN (PATH_MAX < 8192 ? 8192 : PATH_MAX) /* maximum path length */

#ifndef DFLT_PAGER
#define	DFLT_PAGER "more"	/* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160	/* for ! command and commands that use the pager */


#ifndef A_CHARTEXT	/* Should be defined in curses.h */
#define A_CHARTEXT 0xff
#endif

#ifndef color_set
#define color_set(c, o)		attron(COLOR_PAIR(c))
#endif

#if !defined(HAVE_ATTR_T) && defined(_COMPAT_H) /* Not defined for psc */
typedef chtype attr_t;
#endif

#if !defined(HAVE_ATTR_GET) && !defined(NO_ATTR_GET)
#define attr_get(a, p, o)	((void)((a) != 0 && (*(a) = stdscr->_attrs)), \
				(void)((p) != 0 && \
				(*(p) = PAIR_NUMBER(stdscr->_attrs))), OK)
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(strrchr)
#define strrchr rindex
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(strchr)
#define strchr index
#endif

#ifdef SYSV4
size_t	strlen();
#endif

#ifndef FALSE
# define	FALSE	0
# define	TRUE	1
#endif /* !FALSE */

/*
 * ent_ptr holds the row/col # and address type of a cell
 *
 * vf is the type of cell address, 0 non-fixed, or bitwise OR of FIX_ROW or
 *	FIX_COL
 * vp : we just use vp->row or vp->col, vp may be a new cell just for holding
 *	row/col (say in gram.y) or a pointer to an existing cell
 */
struct ent_ptr {
    int vf;
    struct ent *vp;
};

/* holds the beginning/ending cells of a range */
struct range_s {
    struct ent_ptr left, right;
};

/*
 * Some not too obvious things about the flags:
 *    IS_VALID means there is a valid number in v.
 *    IS_LOCKED means that the cell cannot be edited.
 *    IS_LABEL set means it points to a valid constant string.
 *    IS_STREXPR set means expr yields a string expression.
 *    If IS_STREXPR is not set, and expr points to an expression tree, the
 *        expression yields a numeric expression.
 *    So, either v or label can be set to a constant. 
 *        Either (but not both at the same time) can be set from an expression.
 */

#define VALID_CELL(p, r, c) ((p = *ATBL(tbl, r, c)) && \
			     ((p->flags & IS_VALID) || p->label))

/* info for each cell, only alloc'd when something is stored in a cell */
struct ent {
    double v;			/* v && label are set in EvalAll() */
    char *label;
    struct enode *expr;		/* cell's contents */
    short flags;	
    short row, col;
    short nrow, ncol;		/* link to note */
    short nlastrow, nlastcol;
    struct ent *next;		/* next deleted ent (pulled, deleted cells) */
    char *format;		/* printf format for this cell */
    char cellerror;		/* error in a cell? */
};

#define FIX_ROW 1
#define FIX_COL 2

/* stores type of operation this cell will perform */
struct enode {
    int op;
    union {
	int gram_match;         /* some compilers (hp9000ipc) need this */
	double k;		/* constant # */
	struct ent_ptr v;	/* ref. another cell */
	struct range_s r;	/* op is on a range */
	char *s;		/* string part of a cell */
	struct {		/* other cells use to eval()/seval() */
	    struct enode *left, *right;
	    char *s;		/* previous value of @ext function in case */
	} o;			/*	external functions are turned off */
    } e;
};

/* stores a range (left, right) */
struct range {
    struct ent_ptr r_left, r_right;
    char *r_name;			/* possible name for this range */
    struct range *r_next, *r_prev;	/* chained ranges */
    int r_is_range;
};

/* stores a framed range (left, right) */
struct frange {
    struct ent *or_left, *or_right;	/* outer range */
    struct ent *ir_left, *ir_right;	/* inner range */
    struct frange *r_next, *r_prev;	/* chained ranges */
};

/* stores a color range (left, right) */
struct crange {
    struct ent *r_left, *r_right;
    int r_color;
    struct crange *r_next, *r_prev;	/* chained ranges */
};

struct colorpair {
    int fg;
    int bg;
    struct enode *expr;
};

/* stores an abbreviation and its expansion */
struct abbrev {
    char *abbr;
    char *exp;
    struct abbrev *a_next, *a_prev;
};

struct impexfilt {
    char ext[PATHLEN];
    char plugin[PATHLEN];
    char type;
    struct impexfilt *next;
};

/* Use this structure to save the last 'g' command */
struct go_save {
    int		g_type;
    double	g_n;
    char	*g_s;
    int		g_row;
    int		g_col;
    int		g_lastrow;
    int		g_lastcol;
    int		strow;
    int		stcol;
    int		stflag;
    int		errsearch;
};

/* op values */
#define O_VAR 'v'
#define O_CONST 'k'
#define O_ECONST 'E'	/* constant cell w/ an error */
#define O_SCONST '$'
#define REDUCE 0200	/* Or'ed into OP if operand is a range */

#define OP_BASE 256
#define ACOS (OP_BASE + 0)
#define ASIN (OP_BASE + 1)
#define ATAN (OP_BASE + 2)
#define CEIL (OP_BASE + 3)
#define COS (OP_BASE + 4)
#define EXP (OP_BASE + 5)
#define FABS (OP_BASE + 6)
#define FLOOR (OP_BASE + 7)
#define HYPOT (OP_BASE + 8)
#define LOG (OP_BASE + 9)
#define LOG10 (OP_BASE + 10)
#define POW (OP_BASE + 11)
#define SIN (OP_BASE + 12)
#define SQRT (OP_BASE + 13)
#define TAN (OP_BASE + 14)
#define DTR (OP_BASE + 15)
#define RTD (OP_BASE + 16)
#define SUM (OP_BASE + 17)
#define PROD (OP_BASE + 18)
#define AVG (OP_BASE + 19)
#define COUNT (OP_BASE + 20)
#define STDDEV (OP_BASE + 21)
#define MAX (OP_BASE + 22)
#define MIN (OP_BASE + 23)
#define RND (OP_BASE + 24)
#define HOUR (OP_BASE + 25)
#define MINUTE (OP_BASE + 26)
#define SECOND (OP_BASE + 27)
#define MONTH (OP_BASE + 28)
#define DAY (OP_BASE + 29)
#define YEAR (OP_BASE + 30)
#define NOW (OP_BASE + 31)
#define DATE (OP_BASE + 32)
#define FMT (OP_BASE + 33)
#define SUBSTR (OP_BASE + 34)
#define STON (OP_BASE + 35)
#define EQS (OP_BASE + 36)
#define EXT (OP_BASE + 37)
#define ELIST (OP_BASE + 38)	/* List of expressions */
#define LMAX  (OP_BASE + 39)
#define LMIN  (OP_BASE + 40)
#define NVAL (OP_BASE + 41)
#define SVAL (OP_BASE + 42)
#define PV (OP_BASE + 43)
#define FV (OP_BASE + 44)
#define PMT (OP_BASE + 45)
#define STINDEX (OP_BASE + 46)
#define LOOKUP (OP_BASE + 47)
#define ATAN2 (OP_BASE + 48)
#define INDEX (OP_BASE + 49)
#define DTS (OP_BASE + 50)
#define TTS (OP_BASE + 51)
#define ABS (OP_BASE + 52)
#define HLOOKUP (OP_BASE + 53)
#define VLOOKUP (OP_BASE + 54)
#define ROUND (OP_BASE + 55)
#define IF (OP_BASE + 56)
#define FILENAME (OP_BASE + 57)
#define MYROW (OP_BASE + 58)
#define MYCOL (OP_BASE + 59)
#define LASTROW (OP_BASE + 60)
#define LASTCOL (OP_BASE + 61)
#define COLTOA (OP_BASE + 62)
#define UPPER (OP_BASE + 63)
#define LOWER (OP_BASE + 64)
#define CAPITAL (OP_BASE + 65)
#define NUMITER (OP_BASE + 66)
#define ERR_ (OP_BASE + 67)
#define PI_ (OP_BASE + 68)
#define BLACK (OP_BASE + 69)
#define RED (OP_BASE + 70)
#define GREEN (OP_BASE + 71)
#define YELLOW (OP_BASE + 72)
#define BLUE (OP_BASE + 73)
#define MAGENTA (OP_BASE + 74)
#define CYAN (OP_BASE + 75)
#define WHITE (OP_BASE + 76)

/* flag values */
#define IS_VALID     0001
#define IS_CHANGED   0002
#define IS_STREXPR   0004
#define IS_LEFTFLUSH 0010
#define IS_DELETED   0020
#define IS_LOCKED    0040
#define IS_LABEL     0100
#define IS_CLEARED   0200
#define MAY_SYNC     0400

/* cell error (1st generation (ERROR) or 2nd+ (INVALID)) */
#define	CELLOK		0
#define	CELLERROR	1
#define	CELLINVALID	2

#define ctl(c) ((c)&037)
#define ESC 033
#define DEL 0177

/* calculation order */
#define BYCOLS 1
#define BYROWS 2

/* values for showrange for ranges of rows or columns */
#define SHOWROWS 2
#define SHOWCOLS 3

/* tblprint style output for: */
#define	TBL	1		/* 'tbl' */
#define	LATEX	2		/* 'LaTeX' */
#define	TEX	3		/* 'TeX' */
#define	SLATEX	4		/* 'SLaTeX' (Scandinavian LaTeX) */
#define	FRAME	5		/* tblprint style output for FrameMaker */

/* Types for etype() */
#define NUM	1
#define STR	2

#define	GROWAMT	30	/* default minimum amount to grow */

#define	GROWNEW		1	/* first time table */
#define	GROWROW		2	/* add rows */
#define	GROWCOL		3	/* add columns */
#define	GROWBOTH	4	/* grow both */
extern	struct ent ***tbl;	/* data table ref. in vmtbl.c and ATBL() */

extern	char curfile[PATHLEN];
extern	int arg;
extern	int strow, stcol;
extern	int currow, curcol;
extern	int gmyrow, gmycol;	/* globals used for @myrow, @mycol cmds */
extern	int rescol;		/* columns reserved for row numbers */
extern	int savedrow[37], savedcol[37];
extern	int savedstrow[37], savedstcol[37];
extern	int FullUpdate;
extern	int maxrow, maxcol;
extern	int maxrows, maxcols;	/* # cells currently allocated */
extern	int rowsinrange;	/* Number of rows in target range of a goto */
extern	int colsinrange;	/* Number of cols in target range of a goto */
extern	int *fwidth;
extern	int *precision;
extern  int *realfmt;
extern	char *colformat[10];
extern	char *col_hidden;
extern	char *row_hidden;
extern	char line[FBUFLEN];
extern	ssize_t linelim;
extern	int changed;
extern	struct ent *delbuf[DELBUFSIZE];
extern	char *delbuffmt[DELBUFSIZE];
extern	int dbidx;
extern	int qbuf;		/* buffer no. specified by `"' command */
extern	int showsc, showsr;
extern	int showrange;		/* Causes ranges to be highlighted */
extern	int cellassign;
extern	int macrofd;
extern	int cslop;
extern	int usecurses;
extern	int brokenpipe;		/* Set to true if SIGPIPE is received */
extern  char dpoint;	/* country-dependent decimal point from locale */
extern  char thsep;	/* country-dependent thousands separator from locale */
extern	char histfile[PATHLEN];
extern	int lastmx, lastmy;	/* Screen address of the cursor */
extern	int lastcol, lcols;	/* Spreadsheet Column the cursor was in last */
extern	int lastendrow;		/* Last bottom row of screen */
extern	int framerows;		/* Rows in current frame */
extern	int framecols;		/* Columns in current frame */
extern	char mode_ind;		/* Mode indicator */
extern	int	seenerr;

extern	FILE *openfile(char *, size_t, int *, int *);
extern	char *coltoa(int col);
extern	char *findplugin(char *ext, char type);
extern	char *findhome(char *, size_t);
extern	char *r_name(int r1, int c1, int r2, int c2);
extern	void *scxmalloc(size_t n);
extern	void *scxrealloc(void *ptr, size_t n);
extern	char *seval(register struct enode *se);
extern	char *v_name(int row, int col);
extern	double eval(register struct enode *e);
extern	int any_locked_cells(int r1, int c1, int r2, int c2);
extern	int are_colors(void);
extern	int are_frames(void);
extern	int are_ranges(void);
extern	int atocol(char *string, int len);
extern	int creadfile(char *save, int  eraseflg);
extern	int cwritefile(char *fname, int r0, int c0, int rn, int cn);
extern	bool engformat(int fmt, int width, int lprecision, double val,
	char *buf, int buflen);
extern	int etype(register struct enode *e);
extern	int find_range(char *name, int len, struct ent *lmatch,
	struct ent *rmatch, struct range **rng);
extern	bool format(char *fmt, int lprecision, double val, char *buf,
	size_t buflen);
extern	int get_rcqual(int ch);
extern	int growtbl(int rowcol, int toprow, int topcol);
extern	int locked_cell(int r, int c);
extern	int modcheck(char *endstr);
extern	int nmgetch(void);
extern	int plugin_exists(char *name, size_t len, char *path);
extern	int readfile(char *fname, int eraseflg);
extern	int writefile(char *fname, int r0, int c0, int rn, int cn);
extern	int yn_ask(char *msg);
extern	struct abbrev *find_abbr(char *abbrev, int len, struct abbrev **prev);
extern	struct colorpair *cpairs[8];
extern	struct enode *copye(register struct enode *e, int Rdelta, int Cdelta,
	int r1, int c1, int r2, int c2, int transpose);
extern	struct enode *new(int op, struct enode *a1, struct enode *a2);
extern	struct enode *new_const(int op, double a1);
extern	struct enode *new_range(int op, struct range_s a1);
extern	struct enode *new_str(char *s);
extern	struct enode *new_var(int op, struct ent_ptr a1);
extern	struct ent *lookat(int row, int col);
extern	struct crange *find_crange(int row, int col);
extern	struct frange *find_frange(int row, int col);
extern	void EvalAll(void);
extern	void add_crange(struct ent *r_left, struct ent *r_right, int pair);
extern	void add_frange(struct ent *or_left, struct ent *or_right,
	struct ent *ir_left, struct ent *ir_right, int toprows, int bottomrows,
	int leftcols, int rightcols);
extern	void add_range(char *name, struct ent_ptr left, struct ent_ptr right,
	int is_range);
extern	void addplugin(char *ext, char *plugin, char type);
extern	void backcol(int arg);
extern	void backrow(int arg);
extern	void change_color(int pair, struct enode *e);
extern	void checkbounds(int *rowp, int *colp);
extern	void clearent(struct ent *v);
extern	void clean_crange(void);
extern	void clean_frange(void);
extern	void clean_range(void);
extern	void closecol(int arg);
extern	void closefile(FILE *f, int pid, int rfd);
extern	void closerow(int r, int numrow);
extern	void colshow_op(void);
extern	void copy(struct ent *dv1, struct ent *dv2, struct ent *v1,
	struct ent *v2);
extern	void copyent(register struct ent *n, register struct ent *p,
	int dr, int dc, int r1, int c1, int r2, int c2, int transpose);
extern	void decompile(register struct enode *e, int priority);
extern	void deleterow(register int arg);
extern	void del_range(struct ent *left, struct ent *right);
extern	void del_abbr(char *abbrev);
extern	void deraw(int ClearLastLine);
extern	void diesave(void);
extern	void doend(int rowinc, int colinc);
extern	void doformat(int c1, int c2, int w, int p, int r);
extern	void dupcol(void);
extern	void duprow(void);
extern	void doquery(char *s, char *data, int fd);
extern	void dostat(int fd);
extern	void dotick(int tick);
extern	void editexp(int row, int col);
extern	void editfmt(int row, int col);
extern	void edit_mode(void);
extern	void edits(int row, int col);
extern	void editv(int row, int col);
extern	void efree(struct enode *e);
extern	void erase_area(int sr, int sc, int er, int ec, int ignorelock);
extern	void erasedb(void);
extern	void eraser(struct ent *v1, struct ent *v2);
extern	void fgetnum(int r0, int c0, int rn, int cn, int fd);
extern	void fill(struct ent *v1, struct ent *v2, double start, double inc);
extern	void fix_colors(int row1, int col1, int row2, int col2,
	int delta1, int delta2);
extern	void fix_frames(int row1, int col1, int row2, int col2,
	int delta1, int delta2);
extern	void fix_ranges(int row1, int col1, int row2, int col2,
	int delta1, int delta2);
extern	void flush_saved(void);
extern	void formatcol(int arg);
extern	void format_cell(struct ent *v1, struct ent *v2, char *s);
extern	void forwcol(int arg);
extern	void forwrow(int arg);
extern	void free_ent(register struct ent *p, int unlock);
extern	void getexp(int r0, int c0, int rn, int cn, int fd);
extern	void getfmt(int r0, int c0, int rn, int cn, int fd);
extern	void getformat(int col, int fd);
extern	void getnum(int r0, int c0, int rn, int cn, int fd);
extern	void getstring(int r0, int c0, int rn, int cn, int fd);
extern	void go_last(int reverse);
extern	void goraw(void);
extern	void help(void);
extern	void hide_col(int arg);
extern	void hide_row(int arg);
extern	void hidecol(int arg);
extern	void hiderow(int arg);
extern	void initcolor(int colornum);
extern	void initkbd(void);
extern	void ins_in_line(int c);
extern	void ins_string(char *s);
extern	void insert_mode(void);
extern	void insertcol(int arg, int delta);
extern	void insertrow(int arg, int delta);
extern	void kbd_again(void);
extern	void label(register struct ent *v, register char *s, int flushdir);
extern	void let(struct ent *v, struct enode *e);
extern	void list_colors(FILE *f);
extern	void list_ranges(FILE *f);
extern	void lock_cells(struct ent *v1, struct ent *v2);
extern	void markcell(void);
extern	void move_area(int dr, int dc, int sr, int sc, int er, int ec);
extern	void mover(struct ent *d, struct ent *v1, struct ent *v2);
extern	void moveto(int row, int col, int lastrow, int lastcol,
	int cornrow, int corncol);
extern	void toggle_navigate_mode(void);
extern	void num_search(double n, int firstrow, int firstcol, int lastrow,
	int lastcol, int errsearch, int reverse);
extern	void printfile(char *fname, int r0, int c0, int rn, int cn);
extern	void pullcells(int to_insert);
extern	void query(const char *s, char *data);
extern	void read_hist(void);
extern	void remember(int save);
extern	void resetkbd(void);
extern	void rowshow_op(void);
extern	void scxfree(void *p);
extern	void setauto(int i);
extern	void setiterations(int i);
extern	void setorder(int i);
extern	void showcol(int c1, int c2);
extern	void showdr(void);
extern	void showrow(int r1, int r2);
extern	void showstring(char *string, int dirflush, int hasvalue, int row,
	int col, int *nextcolp, int mxcol, int *fieldlenp, int r, int c,
	struct frange *fr, int frightcols, int flcols, int frcols);
extern	void signals(void);
extern	void slet(struct ent *v, struct enode *se, int flushdir);
extern	void sortrange(struct ent *left, struct ent *right, char *criteria);
extern	void startshow(void);
extern	void startdisp(void);
extern	void stopdisp(void);
extern	void str_search(char *s, int firstrow, int firstcol, int lastrow,
	int lastcol, int num, int reverse);
extern	void sync_cranges(void);
extern	void sync_franges(void);
extern	void sync_ranges(void);
extern	void sync_refs(void);
extern	void tblprintfile(char *fname, int r0, int c0, int rn, int cn);
extern	void unlock_cells(struct ent *v1, struct ent *v2);
extern	void update(int anychanged);
extern	void valueize_area(int sr, int sc, int er, int ec);
extern	void write_cells(register FILE *f, int r0, int c0, int rn, int cn,
	int dr, int dc);
extern	void write_colors(FILE *f, int indent);
extern	void write_cranges(FILE *f);
extern	void write_fd(register FILE *f, int r0, int c0, int rn, int cn);
extern	void write_franges(FILE *f);
extern	void write_hist(void);
extern	void write_line(int c);
extern	void write_ranges(FILE *f);
extern	void yank_area(int sr, int sc, int er, int ec);
extern	void yyerror(char *err);
extern	int yylex(void);
extern	int yyparse(void);
extern	int backup_file(char *path);

extern	int modflg;
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
extern	int Crypt;
#endif
extern	char *mdir;
extern	char *autorun;
extern	int skipautorun;
extern	char *fkey[FKEYS];
extern	char *scext;
extern	char *ascext;
extern	char *tbl0ext;
extern	char *tblext;
extern	char *latexext;
extern	char *slatexext;
extern	char *texext;
extern	int scrc;
extern	double prescale;
extern	int extfunc;
extern	int propagation;
extern	int repct;
extern	int calc_order;
extern	int autocalc;
extern	int autolabel;
extern	int autoinsert;
extern	int autowrap;
extern	int optimize;
extern	int numeric;
extern	int showcell;
extern	int showtop;
extern	int color;
extern	int colorneg;
extern	int colorerr;
extern	int braille;
extern	int braillealt;
extern	int dobackups;
extern	int loading;
extern	int getrcqual;
extern	int tbl_style;
extern	int rndtoeven;
extern	char *progname;
#ifdef TRACE
extern	FILE *ftrace;
#endif
extern	int numeric_field;
extern	int craction;
extern	int  pagesize;	/* If nonzero, use instead of 1/2 screen height */
extern	int rowlimit;
extern	int collimit;
#ifdef NCURSES_MOUSE_VERSION
extern MEVENT mevent;
#endif
void gotonote(void);
void center(int, int, int, int);
void rjustify(int, int, int, int);
void ljustify(int, int, int, int);
void yankcol(int);
void yankrow(int);
void list_frames(FILE *);
void yankr(struct ent *, struct ent *);
void dogetkey(void);
void doseval(struct enode *, int, int, int);
void doeval(struct enode *, char *, int, int, int);
void getrange(char *, int);
void getframe(int);
void add_abbr(char *);
void repaint(int, int, int, int, int);
void update(int);
void doshell(void);
#ifdef SIGVOID
void doquit(int);
void time_out(int);
void dump_me(int);
void nopipe(int);
# ifdef	SIGWINCH
void winchg(int);
# endif
#else
int doquit(int);
int time_out(int);
int dump_me(int);
int nopipe(int);
# ifdef	SIGWINCH
int winchg(int);
# endif
#endif
void gohome(void);
void leftlimit(void);
void rightlimit(void);
void gototop(void);
void gotobottom(void);
void mouseon(void);
void mouseoff(void);

#if BSD42 || SYSIII

#ifndef cbreak
#define	cbreak		crmode
#define	nocbreak	nocrmode
#endif

#endif

#if defined(BSD42) || defined(BSD43) && !defined(ultrix)
#define	memcpy(dest, source, len)	bcopy(source, dest, (unsigned int)len);
#define	memset(dest, zero, len)		bzero((dest), (unsigned int)(len));
#else
#include <memory.h>
#endif
