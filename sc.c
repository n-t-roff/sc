/*	SC	A Spreadsheet Calculator
 *		Main driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 7.16 $
 *
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#include <fcntl.h>
#ifndef MSDOS
#include <unistd.h>
#endif
#include <termios.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include "compat.h"
#include "sc.h"
#include "version.h"

#ifndef SAVENAME
#define	SAVENAME "SC.SAVE" /* file name to use for emergency saves */
#endif /* SAVENAME */

static void settcattr(void);
static void scroll_down(void);
static void scroll_up(int);

/* Globals defined in sc.h */

struct ent ***tbl;
int arg = 1;
int strow = 0, stcol = 0;
int currow = 0, curcol = 0;
int savedrow[37], savedcol[37];
int savedstrow[37], savedstcol[37];
int FullUpdate = 0;
int maxrow, maxcol;
int maxrows, maxcols;
int *fwidth;
int *precision;
int *realfmt;
char *col_hidden;
char *row_hidden;
char line[FBUFLEN];
int changed;
struct ent *delbuf[DELBUFSIZE];
char *delbuffmt[DELBUFSIZE];
int dbidx;
int qbuf;	/* buffer no. specified by " command */
int modflg;
int cellassign;
int numeric;
char *mdir;
char *autorun;
int skipautorun;
char *fkey[FKEYS];
char *scext;
char *ascext;
char *tbl0ext;
char *tblext;
char *latexext;
char *slatexext;
char *texext;
int scrc = 0;
int showsc, showsr;	/* Starting cell for highlighted range */
int usecurses = TRUE;	/* Use curses unless piping/redirection or using -q */
int brokenpipe = FALSE;	/* Set to true if SIGPIPE is received */
#ifdef RIGHT_CBUG
int	wasforw	= FALSE;
#endif

char    curfile[PATHLEN];
char    revmsg[80];

/* numeric separators, country-dependent if locale support enabled: */
char	dpoint = '.';	/* decimal point */
char	thsep = ',';	/* thousands separator */

ssize_t  linelim = -1;

int  showtop   = 1;	/* Causes current cell value display in top line  */
int  showcell  = 1;	/* Causes current cell to be highlighted	  */
int  showrange = 0;	/* Causes ranges to be highlighted		  */
int  showneed  = 0;	/* Causes cells needing values to be highlighted  */
int  showexpr  = 0;	/* Causes cell exprs to be displayed, highlighted */
int  shownote  = 0;	/* Causes cells with attached notes to be
			   highlighted					  */
int  braille   = 0;	/* Be nice to users of braille displays		  */
int  braillealt = 0;	/* Alternate mode for braille users		  */

int  autocalc  = 1;	/* 1 to calculate after each update */
int  autolabel = 1;     /* If room, causes label to be created after a define */
int  autoinsert = 0;    /* Causes rows to be inserted if craction is non-zero
			   and the last cell in a row/column of the scrolling
			   portion of a framed range has been filled	  */
int  autowrap = 0;      /* Causes cursor to move to next row/column if craction
			   is non-zero and the last cell in a row/column of
			   the scrolling portion of a framed range has been
			   filled */
int  calc_order = BYROWS;
int  optimize  = 0;	/* Causes numeric expressions to be optimized */
int  tbl_style = 0;	/* headers for T command output */
int  rndtoeven = 0;
int  color     = 0;	/* Use color */
int  colorneg  = 0;	/* Increment color number for cells with negative
			   numbers */
int  colorerr  = 0;	/* Color cells with errors with color 3 */
int  numeric_field = 0; /* Started the line editing with a number */
int  craction = 0;	/* 1 for down, 2 for right */
int  pagesize = 0;	/* If nonzero, use instead of 1/2 screen height */
int  dobackups;         /* Copy current database file to backup file      */
                        /* before overwriting                             */
int  rowlimit = -1;
int  collimit = -1;
int  rowsinrange = 1;
int  colsinrange = DEFWIDTH;

/* a linked list of free [struct ent]'s, uses .next as the pointer */
struct ent *freeents = NULL;

#ifdef VMS
int VMS_read_raw = 0;
#endif

#ifdef NCURSES_MOUSE_VERSION
static int mouse_sel_cell(int);

MEVENT mevent;
#endif

/* return a pointer to a cell's [struct ent *], creating if needed */
struct ent *
lookat(int row, int col)
{
    register struct ent **pp;

    checkbounds(&row, &col);
    pp = ATBL(tbl, row, col);
    if (*pp == NULL) {
        if (freeents != NULL) {
	    *pp = freeents;
	    (*pp)->flags &= ~IS_CLEARED;
	    (*pp)->flags |= MAY_SYNC;
	    freeents = freeents->next;
	} else
	    *pp = scxmalloc(sizeof(struct ent));
	if (row > maxrow) maxrow = row;
	if (col > maxcol) maxcol = col;
	(*pp)->label = (char *)0;
	(*pp)->row = row;
	(*pp)->col = col;
	(*pp)->nrow = -1;
	(*pp)->ncol = -1;
	(*pp)->flags = MAY_SYNC;
	(*pp)->expr = (struct enode *)0;
	(*pp)->v = (double) 0.0;
	(*pp)->format = (char *)0;
	(*pp)->cellerror = CELLOK;
	(*pp)->next = NULL;
    }
    return (*pp);
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
void
free_ent(register struct ent *p, int unlock)
{
    p->next = delbuf[dbidx];
    delbuf[dbidx] = p;
    p->flags |= IS_DELETED;
    if (unlock)
	p->flags &= ~IS_LOCKED;
}

/* free deleted cells */
void
flush_saved(void) {
    register struct ent *p;
    register struct ent *q;

    if (dbidx < 0)
	return;
    if ((p = delbuf[dbidx])) {
	scxfree(delbuffmt[dbidx]);
	delbuffmt[dbidx] = NULL;
    }
    while (p) {
	(void) clearent(p);
	q = p->next;
	p->next = freeents;	/* put this ent on the front of freeents */
	freeents = p;
	p = q;
    }
    delbuf[dbidx--] = NULL;
}

char	*progname;
int	Vopt;
#ifdef TRACE
FILE	*ftrace;
#endif

int
main (int argc, char  **argv)
{
    int     inloop = 1;
    register int   c;
    int     edistate = -1;
    int     narg;
    int     nedistate;
    int	    running;
    char    *revi;
    int	    anychanged = FALSE;
    int     tempx, tempy; 	/* Temp versions of curx, cury */

    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */

    int mopt = 0;
    int oopt = 0;
    int nopt = 0;
    int copt = 0; 
    int ropt = 0;
    int Copt = 0; 
    int Ropt = 0;
    int eopt = 0;
    int popt = 0;
    int qopt = 0;
    int Mopt = 0;

    Vopt = 0;

#ifdef MSDOS
    if ((revi = strrchr(argv[0], '\\')) != NULL)
#else
#ifdef VMS
    if ((revi = strrchr(argv[0], ']')) != NULL)
#else
    if ((revi = strrchr(argv[0], '/')) != NULL)
#endif
#endif
	progname = revi+1;
    else
	progname = argv[0];

#ifdef TRACE
    if (!(ftrace = fopen(TRACE, "w"))) {
	fprintf(stderr, "%s: fopen(%s, 'w') failed: %s\n",
	    progname, TRACE, strerror(errno));
	exit(1);
    }
#endif

    while ((c = getopt(argc, argv, "axmoncrCReP:W:vqM")) != EOF) {
    	switch (c) {
	    case 'a':
		    skipautorun = 1;
		    break;
	    case 'x':
#if defined(VMS) || defined(MSDOS) || !defined(CRYPT_PATH)
		    (void) fprintf(stderr, "Crypt not available\n");
		    exit (1);
#else 
		    Crypt = 1;
#endif
		    break;
	    case 'm':
		    mopt = 1;
		    break;
	    case 'o':
		    oopt = 1;
		    break;
	    case 'n':
		    nopt = 1;
		    break;
	    case 'c':
		    copt = 1;
		    break;
	    case 'r':
		    ropt = 1;
		    break;
	    case 'C':
		    Copt = 1;
		    craction = CRCOLS;
		    break;
	    case 'R':
		    Ropt = 1;
		    craction = CRROWS;
		    break;
	    case 'e':
		    rndtoeven = 1;
		    eopt = 1;
		    break;
	    case 'P':
	    case 'W':
		    popt = 1;
	    case 'v':
		    break;
	    case 'q':
		    qopt = 1;
		    break;
	    case 'M':
		    Mopt = 1;
		    break;
	    default:
		    exit (1);
	}
    }

    if (!isatty(STDOUT_FILENO) || popt || qopt) usecurses = FALSE;
    startdisp();
    signals();
    settcattr();
    read_hist();

    /* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(GROWNEW, 0, 0)) {
     	stopdisp();
	exit (1);
    }

    /*
     * Build revision message for later use:
     */

    if (popt)
	*revmsg = '\0';
    else {
	const char *revi;
	strlcpy(revmsg, progname, sizeof revmsg);
	for (revi = rev; (*revi++) != ':'; );	/* copy after colon */
	strlcat(revmsg, revi, sizeof revmsg);
	revmsg[strlen(revmsg) - 2] = 0;		/* erase last character */
	strlcat(revmsg, ":  Type '?' for help.", sizeof revmsg);
    }

#ifdef MSDOS
    if (optind < argc)
#else 
    if (optind < argc && !strcmp(argv[optind], "--"))
	optind++;
    if (optind < argc && argv[optind][0] != '|' &&
	    strcmp(argv[optind], "-"))
#endif /* MSDOS */
	strlcpy(curfile, argv[optind], sizeof curfile);
    for (dbidx = DELBUFSIZE - 1; dbidx >= 0; ) {
	delbuf[dbidx] = NULL;
	delbuffmt[dbidx--] = NULL;
    }
    if (usecurses && has_colors())
	initcolor(0);

    if (optind < argc) {
	if (!readfile(argv[optind], 1) && (optind == argc - 1))
	    error("New file: \"%s\"", curfile);
	EvalAll();
	optind++;
    } else
	erasedb();

    while (optind < argc) {
	(void) readfile(argv[optind], 0);
	optind++;
    }

    savedrow[0] = currow;
    savedcol[0] = curcol;
    savedstrow[0] = strow;
    savedstcol[0] = stcol;
    EvalAll();

    if (!(popt || isatty(STDIN_FILENO)))
	(void) readfile("-", 0);

    if (qopt) {
	stopdisp();
	exit (0);
    }

    if (usecurses)
	clearok(stdscr, TRUE);
    EvalAll();

    if (mopt)
	autocalc = 0;
    if (oopt)
	optimize = 1;
    if (nopt)
	numeric = 1;
    if (copt)
	calc_order = BYCOLS;
    if (ropt)
	calc_order = BYROWS;
    if (Copt)
	craction = CRCOLS;
    if (Ropt)
	craction = CRROWS;
    if (eopt)
	rndtoeven = 1;
    if (Mopt)
	mouseon();
    if (popt) {
	char *redraw = NULL;
	int o;

#ifdef BSD43
	optreset = 1;
#endif
	optind = 1;
	stopdisp();
	while ((o = getopt(argc, argv, "axmoncrCReP:W:vq")) != EOF) {
	    switch (o) {
		case 'v':
		    Vopt = 1;
		    break;
		case 'P':
		    if (*optarg == '/') {
			int in, out;

			in = dup(STDIN_FILENO);
			out = dup(STDOUT_FILENO);
			freopen("/dev/tty", "r", stdin);
			freopen("/dev/tty", "w", stdout);
			usecurses = TRUE;
			startdisp();
			if (has_colors()) {
			    initcolor(0);
			    bkgd(COLOR_PAIR(1) | ' ');
			}
			clearok(stdscr, TRUE);
			FullUpdate++;
			linelim = 0;
			*line = '\0';
			if (mode_ind != 'v')
			    write_line(ctl('v'));
			error("Select range:");
			update(1);
			while (!linelim) {
			    int c_;

			    switch (c_ = nmgetch()) {
				case '.':
				case ':':
				case ctl('i'):
				    if (!showrange) {
					write_line(c_);
					break;
				    }
		    		    /* else drop through */
				case ctl('m'):
				    strlcpy(line, "put ", sizeof line);
				    linelim = 4;
				    write_line('.');
				    if (showrange)
					write_line('.');
				    strlcat(line, optarg, sizeof line);
				    break;
				case ESC:
				case ctl('g'):
				case 'q':
				    linelim = -1;
				    break;
				case ctl('l'):
				    FullUpdate++;
				    clearok(stdscr, 1);
				    break;
				default:
				    write_line(c_);
				    break;
			    }
			    /* goto switches to insert mode when done, so we
			     * have to switch back.
			     */
			    if (mode_ind == 'i')
				write_line(ctl('v'));
			    CLEAR_LINE;
			    update(1);
			}
			stopdisp();
			dup2(in, STDIN_FILENO);
			dup2(out, STDOUT_FILENO);
			close(in);
			close(out);
			redraw = "recalc\nredraw\n";
		    } else {
			strlcpy(line, "put ", sizeof line);
			linelim = 4;
			strlcat(line, optarg, sizeof line);
		    }
		    if (linelim > 0) {
			linelim = 0;
			yyparse();
		    }
		    Vopt = 0;
		    break;
		case 'W':
		    strlcpy(line, "write ", sizeof line);
		    strlcat(line, optarg, sizeof line);
		    linelim = 0;
		    yyparse();
		    break;
		default:
		    break;
	    }
	}
	if (redraw) fputs(redraw, stdout);
	exit (0);
    }

    if (!isatty(STDOUT_FILENO)) {
	stopdisp();
	write_fd(stdout, 0, 0, maxrow, maxcol);
	exit (0);
    }

    modflg = 0;
    cellassign = 0;
#ifdef VENIX
    setbuf(stdin, NULL);
#endif

    while (inloop) { running = 1;
    while (running) {
	nedistate = -1;
	narg = 1;
	if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate))
	{
	    EvalAll();
	    if (changed)		/* if EvalAll changed or was before */
		anychanged = TRUE;
	    changed = 0;
	}
	else		/* any cells change? */
	if (changed)
	    anychanged = TRUE;

	update(anychanged);
	anychanged = FALSE;
#ifndef SYSV3	/* HP/Ux 3.1 this may not be wanted */
	(void) refresh(); /* 5.3 does a refresh in getch */ 
#endif
	c = nmgetch();
	getyx(stdscr, tempy, tempx);
	(void) move(1, 0);
	(void) clrtoeol();
	(void) move(tempy, tempx);
	seenerr = 0;
	showneed = 0;	/* reset after each update */
	showexpr = 0;
	shownote = 0;

	/*
	 * there seems to be some question about what to do w/ the iscntrl
	 * some BSD systems are reportedly broken as well
	 */
	/* if ((c < ' ') || ( c == DEL ))   how about international here ? PB */
#if	pyr
	    if(iscntrl(c) || (c >= 011 && c <= 015))	/* iscntrl broken in OSx4.1 */
#else
	    if ((isascii(c) && (iscntrl(c) || (c == 020))) ||	/* iscntrl broken in OSx4.1 */
			c == KEY_END || c == KEY_BACKSPACE)
#endif
	    switch(c) {
#ifdef SIGTSTP
		case ctl('z'):
		    (void) deraw(1);
		    (void) kill(0, SIGTSTP); /* Nail process group */

		    /* the pc stops here */

		    (void) goraw();
		    break;
#endif
		case ctl('r'):
		    showneed = 1;
		case ctl('l'):
		    FullUpdate++;
		    (void) clearok(stdscr,1);
		    break;
		case ctl('x'):
		    FullUpdate++;
		    showexpr = 1;
		    (void) clearok(stdscr,1);
		    break;
		default:
		    error ("No such command (^%c)", c + 0100);
		    break;
		case ctl('b'):
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    backrow(arg * ps);
		    strow = strow - (arg * ps);
		    if (strow < 0) strow = 0;
		    FullUpdate++;
		    }
		    break;
		case ctl('c'):
		    running = 0;
		    break;

		case KEY_END:
		case ctl('e'):
		    if (linelim < 0 || mode_ind == 'v') {
			switch (c = nmgetch()) {
			    case KEY_UP:
			    case ctl('p'): case 'k':	doend(-1, 0);	break;

			    case KEY_DOWN:
			    case ctl('n'): case 'j':	doend( 1, 0);	break;

			    case KEY_LEFT:
			    case KEY_BACKSPACE:
			    case ctl('h'): case 'h':	doend( 0,-1);	break;

			    case KEY_RIGHT:
			    case ' ':
			    case ctl('i'): case 'l':	doend( 0, 1);	break;

			    case ctl('e'):
			    case ctl('y'):
				while (c == ctl('e') || c == ctl('y')) {
				    int x = arg;

				    while (arg) {
					if (c == ctl('e')) {
					    scroll_down();
					} else {
					    scroll_up(x);
					}
					arg--;
				    }
				    FullUpdate++;
				    update(0);
				    arg++;
				    c = nmgetch();
				}
				ungetch(c);
				break;

			    case ESC:
			    case ctl('g'):
				break;

			    default:
				error("Invalid ^E command");
				break;
			}
		    } else
			write_line(ctl('e'));
		    break;

		case ctl('y'):
		    while (c == ctl('e') || c == ctl('y')) {
			int x = arg;

			while (arg) {
			    if (c == ctl('e')) {
				scroll_down();
			    } else {
				scroll_up(x);
			    }
			    arg--;
			}
			FullUpdate++;
			update(0);
			arg++;
			c = nmgetch();
		    }
		    ungetch(c);
		    break;

		case ctl('f'):
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    forwrow(arg * ps);
		    strow = strow + (arg * ps);
		    FullUpdate++;
		    }
		    break;

		case ctl('g'):
		    showrange = 0;
		    linelim = -1;
		    (void) move(1, 0);
		    (void) clrtoeol();
		    break;

		case ESC:	/* ctl('[') */
		    write_line(ESC);
		    break;

		case ctl('d'):
		    write_line(ctl('d'));
		    break;

		case KEY_BACKSPACE:
		case DEL:
		case ctl('h'):
		    if (linelim < 0) {	/* not editing line */
			backcol(arg);	/* treat like ^B    */
			break;
		    }
		    write_line(ctl('h'));
		    break;

		case ctl('i'): 		/* tab */
		    if (linelim < 0) {	/* not editing line */
			forwcol(arg);
			break;
		    }
		    write_line(ctl('i'));
		    break;

		case ctl('m'):
		case ctl('j'):
		    write_line(ctl('m'));
		    break;

		case ctl('n'):
		    c = craction;
		    if (numeric_field) {
			craction = 0;
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    craction = c;
		    if (linelim < 0) {
			forwrow(arg);
			break;
		    }
		    write_line(ctl('n'));
		    break;

		case ctl('p'):
		    c = craction;
		    if (numeric_field) {
			craction = 0;
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    craction = c;
		    if (linelim < 0) {
			backrow(arg);
			break;
		    }
		    write_line(ctl('p'));
		    break;

		case ctl('q'):
		    break;	/* ignore flow control */

		case ctl('s'):
		    break;	/* ignore flow control */

		case ctl('t'):
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
		    error(
"Toggle: a:auto,c:cell,e:ext funcs,n:numeric,t:top,x:encrypt,$:pre-scale,<MORE>");
#else 				/* no encryption available */
		    error(
"Toggle: a:auto,c:cell,e:ext funcs,n:numeric,t:top,$:pre-scale,<MORE>");
#endif
		    if (braille) move(1, 0);
		    (void) refresh();

		    switch (nmgetch()) {
			case 'a': case 'A':
			case 'm': case 'M':
			    autocalc ^= 1;
			    error("Automatic recalculation %sabled.",
				autocalc ? "en":"dis");
			    break;
			case 'o': case 'O':
			    optimize ^= 1;
			    error("%sptimize expressions upon entry.",
				optimize ? "O":"Do not o");
			    break;
			case 'n':
			    numeric = (!numeric);
			    error("Numeric input %sabled.",
				    numeric ? "en" : "dis");
			    break;
			case 't': case 'T':
			    showtop = (!showtop);
			    error("Top line %sabled.", showtop ? "en" : "dis");
			    break;
			case 'c':
			    showcell = (!showcell);
			    repaint(lastmx, lastmy, fwidth[lastcol], 0, 0);
			    error("Cell highlighting %sabled.",
				    showcell ? "en" : "dis");
			    --modflg;	/* negate the modflg++ */
			    break;
			case 'b':
			    braille ^= 1;
			    error("Braille enhancement %sabled.",
				    braille ? "en" : "dis");
			    --modflg;	/* negate the modflg++ */
			    break;
			case 's':
			    cslop ^= 1;
			    error("Color slop %sabled.",
				    cslop ? "en" : "dis");
			    break;
			case 'C':
			    color = !color;
			    if (has_colors()) {
				if (color) {
				    attron(COLOR_PAIR(1));
				    bkgd(COLOR_PAIR(1) | ' ');
				} else {
				    attron(COLOR_PAIR(0));
				    bkgd(COLOR_PAIR(0) | ' ');
				}
			    }
			    error("Color %sabled.", color ? "en" : "dis");
			    break;
			case 'N':
			    colorneg = !colorneg;
			    error("Color changing of negative numbers %sabled.",
				    colorneg ? "en" : "dis");
			    break;
			case 'E':
			    colorerr = !colorerr;
			    error("Color changing of cells with errors %sabled.",
				    colorerr ? "en" : "dis");
			    break;
			case 'x': case 'X':
#if defined(VMS) || defined(MSDOS) || !defined(CRYPT_PATH)
			    error("Encryption not available.");
#else 
			    Crypt = (! Crypt);
			    error("Encryption %sabled.", Crypt? "en" : "dis");
#endif
			    break;
			case 'l': case 'L':
			    autolabel = (!autolabel);
			    error("Autolabel %sabled.",
				   autolabel? "en" : "dis");
			    break;
			case '$':
			    if (prescale == 1.0) {
				error("Prescale enabled.");
				prescale = 0.01;
			    } else {
				prescale = 1.0;
				error("Prescale disabled.");
			    }
			    break;
			case 'e':
			    extfunc = (!extfunc);
			    error("External functions %sabled.",
				    extfunc? "en" : "dis");
			    break;
			case ESC:
			case ctl('g'):
			    CLEAR_LINE;
			    --modflg;	/* negate the modflg++ */
			    break;
			case 'r': case 'R':
			    error("Which direction after return key?");
			    switch(nmgetch()) {
				case ctl('m'):
				    craction = 0;
				    error("No action after new line");
				    break;
				case 'j':
				case ctl('n'):
				case KEY_DOWN:
				    craction = CRROWS;
				    error("Down row after new line");
				    break;
				case 'l':
				case ' ':
				case KEY_RIGHT:
				    craction = CRCOLS;
				    error("Right column after new line");
				    break;
				case ESC:
				case ctl('g'):
				    CLEAR_LINE;
				    break;
				default:
				    error("Not a valid direction");
			    }
			    break;
			case 'i': case 'I':
			    autoinsert = (!autoinsert);
			    error("Autoinsert %sabled.",
				   autoinsert? "en" : "dis");
			    break;
			case 'w': case 'W':
			    autowrap = (!autowrap);
			    error("Autowrap %sabled.",
				   autowrap? "en" : "dis");
			    break;
			case 'z': case 'Z':
			    rowlimit = currow;
			    collimit = curcol;
			    error("Row and column limits set");
			    break;
			default:
			    error("Invalid toggle command");
			    --modflg;	/* negate the modflg++ */
		    }
		    FullUpdate++;
		    modflg++;
		    break;

		case ctl('u'):
		    narg = arg * 4;
		    nedistate = 1;
		    break;

		case ctl('v'):	/* switch to navigate mode, or if already *
				 * in navigate mode, insert variable name */
		    if (linelim >= 0)
		        write_line(ctl('v'));
		    break;

		case ctl('w'):	/* insert variable expression */
		    if (linelim >= 0)  {
			static	char *temp = NULL, *temp1 = NULL;
			static	unsigned	templen = 0;
			int templim;

			/* scxrealloc will scxmalloc if needed */
			if (strlen(line)+1 > templen) {
			    templen = strlen(line)+40;

			    temp = scxrealloc(temp, templen);
			    temp1= scxrealloc(temp1, templen);
			}
			strlcpy(temp, line, templen);
			templim = linelim;
			linelim = 0;		/* reset line to empty	*/
			editexp(currow,curcol);
			strlcpy(temp1, line, templen);
			strlcpy(line, temp, sizeof line);
			linelim = templim;
			ins_string(temp1);
		    }
		    break;

		case ctl('a'):
		    if (linelim >= 0)
			write_line(c);
		    else {
			remember(0);
			currow = 0;
			curcol = 0;
			rowsinrange = 1;
			colsinrange = fwidth[curcol];
			remember(1);
			FullUpdate++;
		    }
		    break;
		case '\035':	/* ^] */
		    if (linelim >= 0)
			write_line(c);
		    break;

	    } /* End of the control char switch stmt */
	else if (isascii(c) && isdigit(c) && ((!numeric && linelim < 0) ||
		(linelim >= 0 && (mode_ind == 'e' || mode_ind == 'v')) ||
		edistate >= 0)) {
	    /* we got a leading number */
	    if (edistate != 0) {
		/* First char of the count */
		if (c == '0') {    /* just a '0' goes to left col */
		    if (linelim >= 0)
			write_line(c);
		    else
			leftlimit();
		} else {
		    nedistate = 0;
		    narg = c - '0';
		}
	    } else {
		/* Succeeding count chars */
		nedistate = 0;
		narg = arg * 10 + (c - '0');
	    }
	} else if (c == KEY_F(1) && !fkey[c - KEY_F0 - 1]) {
	    deraw(1);
	    system("man sc");
	    goraw();
	    clear();
	} else if (linelim >= 0) {
	    /* Editing line */
	    switch (c) {
		case ')':
		case ',':
		    if (showrange)
			showdr();
		    break;
		default:
		    break;
	    }
	    write_line(c);

	} else if (!numeric && ( c == '+' || c == '-' )) {
	    /* increment/decrement ops */
	    register struct ent *p = *ATBL(tbl, currow, curcol);
	    if (!p || !(p->flags & IS_VALID)) {
		if (c == '+') {
		    editv(currow, curcol);
		    linelim = strlen(line);
		    insert_mode();
		    write_line(ctl('v'));
		}
		continue;
	    }
	    if (p->expr && !(p->flags & IS_STREXPR)) {
		error("Can't increment/decrement a formula\n");
		continue;
	    }
	    FullUpdate++;
	    modflg++;
	    if (c == '+')
	    	p->v += (double) arg;
	    else
		p->v -= (double) arg;
	} else if (c > KEY_F0 && c <= KEY_F(FKEYS)) {
	    /* a function key was pressed */
	    if (fkey[c - KEY_F0 - 1]) {
		char *tpp;

		insert_mode();
		strlcpy(line, fkey[c - KEY_F0 - 1], sizeof line);
		linelim = 0;
		for (tpp = line; *tpp != '\0'; tpp++)
		    if (*tpp == '\\' && *(tpp + 1) == '"')
			memmove(tpp, tpp + 1, strlen(tpp));
		for (tpp = line; *tpp != '\0'; tpp++) {
		    char mycell[9];
		    size_t l;
		    l = strlcpy(mycell, coltoa(curcol), sizeof mycell);
		    snprintf(mycell + l, sizeof(mycell) - l, "%d", currow);
		    if (*tpp == '$' && *(tpp + 1) == '$') {
			memmove(tpp + strlen(mycell), tpp + 2, strlen(tpp + 1));
			memcpy(tpp, mycell, strlen(mycell));
			tpp += strlen(mycell);
		    }
		}
		write_line(ctl('m'));
	    }
	} else
	    /* switch on a normal command character */
	    switch (c) {
		case ':':
		    if (linelim >= 0)
			write_line(':');
		    break;	/* Be nice to vi users */

		case '@':
		    EvalAll();
		    changed = 0;
		    anychanged = TRUE;
		    break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '.':
		    if (locked_cell(currow, curcol))
			break;
		    /* set mark 0 */
		    savedrow[27] = currow;
		    savedcol[27] = curcol;
		    savedstrow[27] = strow;
		    savedstcol[27] = stcol;

		    numeric_field = 1;
		    snprintf(line, sizeof line, "let %s = %c",
			    v_name(currow, curcol), c);
		    linelim = strlen(line);
		    insert_mode();
		    break;

		case '+':
		case '-':
		    if (!locked_cell(currow, curcol)) {
			struct ent *p = lookat(currow, curcol);
			/* set mark 0 */
			savedrow[27] = currow;
			savedcol[27] = curcol;
			savedstrow[27] = strow;
			savedstcol[27] = stcol;

			numeric_field = 1;
			editv(currow, curcol);
			linelim = strlen(line);
			insert_mode();
			if (c == '-' || p->flags & IS_VALID)
			    write_line(c);
			else
			    write_line(ctl('v'));
		    }
		    break;

		case '=':
		    if (locked_cell(currow, curcol))
			break;
		    /* set mark 0 */
		    savedrow[27] = currow;
		    savedcol[27] = curcol;
		    savedstrow[27] = strow;
		    savedstcol[27] = stcol;

		    snprintf(line, sizeof line, "let %s = ", v_name(currow, curcol));
		    linelim = strlen(line);
		    insert_mode();
		    break;

		case '!':
		    doshell();
		    break;

		/*
		 * Range commands:
		 */

		case 'r':
		    error(
"Range: x:erase v:value c:copy f:fill d:def l:lock U:unlock S:show u:undef F:fmt");
		    if (braille) move(1, 0);
		    (void) refresh();

		    c = nmgetch();
		    CLEAR_LINE;
		    switch (c) {
		    case 'l':
			snprintf(line, sizeof line, "lock [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'U':
			snprintf(line, sizeof line, "unlock [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'c':
			snprintf(line, sizeof line, "copy [dest_range src_range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'm':
			snprintf(line, sizeof line, "move [destination src_range] %s ",
				v_name(currow, curcol));
			linelim = strlen(line);
			insert_mode();
		        write_line(ctl('v'));
			break;
		    case 'x':
			snprintf(line, sizeof line, "erase [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'y':
			snprintf(line, sizeof line, "yank [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'v':
			snprintf(line, sizeof line, "value [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'f':
			snprintf(line, sizeof line, "fill [range start inc] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'd':
			snprintf(line, sizeof line, "define [string range] \"");
			linelim = strlen(line);
			insert_mode();
			break;
		    case 'u':
			snprintf(line, sizeof line, "undefine [range] ");
			linelim = strlen(line);
			insert_mode();
			break;
		    case 'r':
			error("frame (top/bottom/left/right/all/unframe)");
			if (braille) move(1, 0);
			refresh();
			linelim = 0;
			c = nmgetch();
			CLEAR_LINE;
			switch (c) {
			    case 't':
				snprintf(line, sizeof line, "frametop [<outrange> rows] ");
				break;
			    case 'b':
				snprintf(line, sizeof line, "framebottom [<outrange> rows] ");
				break;
			    case 'l':
				snprintf(line, sizeof line, "frameleft [<outrange> cols] ");
				break;
			    case 'r':
				snprintf(line, sizeof line, "frameright [<outrange> cols] ");
				break;
			    case 'a':
				snprintf(line, sizeof line, "frame [<outrange> inrange] ");
				break;
			    case 'u':
				snprintf(line, sizeof line, "unframe [<range>] ");
				break;
			    case ESC:
			    case ctl('g'):
				linelim = -1;
				break;
			    default:
				error("Invalid frame command");
				linelim = -1;
				break;
			}
			if (linelim == 0) {
			    linelim = strlen(line);
			    insert_mode();
			}
			if (c == 'a' || c == 'u')
			    startshow();
			break;
		    case 's':
			snprintf(line, sizeof line, "sort [range \"criteria\"] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'C':
			snprintf(line, sizeof line, "color [range color#] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case 'S':
			/* Show color definitions and various types of
			 * ranges
			 */
			if (!are_ranges() && !are_frames() && !are_colors()) {
			    error("Nothing to show");
			} else {
			    FILE *f;
			    int pid;
			    char px[MAXCMD];
			    char *pager;

			    strlcpy(px, "| ", sizeof px);
			    if (!(pager = getenv("PAGER")))
				pager = DFLT_PAGER;
			    strlcat(px, pager, sizeof px);
			    f = openfile(px, sizeof px, &pid, NULL);
			    if (!f) {
				error("Can't open pipe to %s", pager);
				break;
			    }
			    fprintf(f, "Named Ranges:\n=============\n\n");
			    if (!brokenpipe) list_ranges(f);
			    if (!brokenpipe)
				fprintf(f, "\n\nFrames:\n=======\n\n");
			    if (!brokenpipe) list_frames(f);
			    if (!brokenpipe)
				fprintf(f, "\n\nColors:\n=======\n\n");
			    if (!brokenpipe) list_colors(f);
			    closefile(f, pid, 0);
			}
			break;
		    case 'F':
			snprintf(line, sizeof line, "fmt [range \"format\"] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case '{':
			snprintf(line, sizeof line, "leftjustify [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case '}':
			snprintf(line, sizeof line, "rightjustify [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case '|':
			snprintf(line, sizeof line, "center [range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    case ESC:
		    case ctl('g'):
			break;
		    default:
			error("Invalid region command");
			break;
		    }
		    break;

		case '~':
		    snprintf(line, sizeof line, "abbrev \"");
		    linelim = strlen(line);
		    insert_mode();
		    break;

		case '"':
		    error("Select buffer (a-z or 0-9):");
		    if ((c=nmgetch()) == ESC || c == ctl('g')) {
			CLEAR_LINE;
		    } else if (c >= '0' && c <= '9') {
			qbuf = c - '0' + (DELBUFSIZE - 10);
			CLEAR_LINE;
		    } else if (c >= 'a' && c <= 'z') {
			qbuf = c - 'a' + (DELBUFSIZE - 36);
			CLEAR_LINE;
		    } else if (c == '"') {
			qbuf = 0;
			CLEAR_LINE;
		    } else
			error("Invalid buffer");
		    break;

		/*
		 * Row/column commands:
		 */

		case KEY_IC:
		case 'i':
		case 'o':
		case 'a':
		case 'd':
		case 'y':
		case 'p':
		case 'v':
		case 's':
		case 'Z':
		    {
			int rcqual;

			if (!(rcqual = get_rcqual(c))) {
			    error("Invalid row/column command");
			    break;
			}

			CLEAR_LINE;	/* clear line */

			if (rcqual == ESC || rcqual == ctl('g'))
			    break;

			switch (c) {

			    case 'i':
				if (rcqual == 'r')	insertrow(arg, 0);
				else			insertcol(arg, 0);
				break;

			    case 'o':
				if (rcqual == 'r')	insertrow(arg, 1);
				else			insertcol(arg, 1);
				break;

			    case 'a':
				if (rcqual == 'r')	while (arg--) duprow();
				else			while (arg--) dupcol();
				break;

			    case 'd':
				if (rcqual == 'r')	deleterow(arg);
				else			closecol(arg);
				break;

			    case 'y':
				if (rcqual == 'r')	yankrow(arg);
				else			yankcol(arg);
				break;

			    case 'p':
				if (rcqual == '.') {
				    snprintf(line, sizeof line, "pullcopy ");
				    linelim = strlen(line);
				    insert_mode();
				    startshow();
				    break;
				}
				while (arg--)		pullcells(rcqual);
				break;

			    /*
			     * turn an area starting at currow/curcol into
			     * constants vs expressions - not reversable
			     */
			    case 'v':
				if (rcqual == 'r') {
				    struct frange *fr;

				    if ((fr = find_frange(currow, curcol)))
					valueize_area(currow, fr->or_left->col,
						currow + arg - 1,
						fr->or_right->col);
				    else
					valueize_area(currow, 0,
						currow + arg - 1, maxcol);
				} else
				    valueize_area(0, curcol,
					    maxrow, curcol + arg - 1);
				modflg++;
				break;

			    case 'Z':
				switch (rcqual) {
				    case 'r':	hiderow(arg);		break;
				    case 'c':	hidecol(arg);		break;
				    case 'Z':	if (modflg && curfile[0]) {
						    writefile(curfile, 0, 0,
							    maxrow, maxcol);
						    running = 0;
						} else if (modflg) {
						    error("No file name.");
						} else
						    running = 0;
						break;
				}
				break;

			    case 's':
			    /* special case; no repeat count */

				if (rcqual == 'r')	rowshow_op();
				else			colshow_op();
				break;
			}
			break;
		    }

		case '$':
		    rightlimit();
		    break;
		case '#':
		    gotobottom();
		    break;
		case 'w':
		    {
		    register struct ent *p;

		    while (--arg>=0) {
			do {
			    if (curcol < maxcols - 1)
				curcol++;
			    else {
				if (currow < maxrows - 1) {
				    while(++currow < maxrows - 1 &&
					    row_hidden[currow])
					;
				    curcol = 0;
				} else {
				    error("At end of table");
				    break;
				}
			    }
			} while(col_hidden[curcol] ||
				!VALID_CELL(p, currow, curcol));
		    }
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    break;
		    }
		case 'b':
		    {
		    register struct ent *p;

		    while (--arg>=0) {
			do {
			    if (curcol) 
				curcol--;
			    else {
				if (currow) {
				    while(--currow && row_hidden[currow])
					;
				    curcol = maxcols - 1;
				} else {
				    error("At start of table");
				    break;
				}
			    }
			} while (col_hidden[curcol] ||
				!VALID_CELL(p, currow, curcol));
		    }
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    break;
		    }
		case '^':
		    gototop();
		    break;
#ifdef KEY_HELP
		case KEY_HELP:
#endif
		case '?':
		    help();
		    break;
		case '\\':
		    if (!locked_cell(currow, curcol)) {
			/* set mark 0 */
			savedrow[27] = currow;
			savedcol[27] = curcol;
			savedstrow[27] = strow;
			savedstcol[27] = stcol;

			snprintf(line, sizeof line, "label %s = \"",
				v_name(currow, curcol));
			linelim = strlen(line);
			insert_mode();
		    }
		    break;

		case '<':
		    if (!locked_cell(currow, curcol)) {
			/* set mark 0 */
			savedrow[27] = currow;
			savedcol[27] = curcol;
			savedstrow[27] = strow;
			savedstcol[27] = stcol;

			snprintf(line, sizeof line, "leftstring %s = \"",
				v_name(currow, curcol));
			linelim = strlen(line);
			insert_mode();
		    }
		    break;

		case '>':
		    if (!locked_cell(currow, curcol)) {
			/* set mark 0 */
			savedrow[27] = currow;
			savedcol[27] = curcol;
			savedstrow[27] = strow;
			savedstcol[27] = stcol;

		       snprintf(line, sizeof line, "rightstring %s = \"",
			      v_name(currow, curcol));
		       linelim = strlen(line);
		       insert_mode();
		    }
		    break;
		case '{':
		    {
			struct ent *p = *ATBL(tbl, currow, curcol);

			if (p && p->label)
			    ljustify(currow, curcol, currow, curcol);
			else
			    error("Nothing to justify");
			break;
		    }
		case '}':
		    {
			struct ent *p = *ATBL(tbl, currow, curcol);

			if (p && p->label)
			    rjustify(currow, curcol, currow, curcol);
			else
			    error("Nothing to justify");
			break;
		    }
		case '|':
		    {
			struct ent *p = *ATBL(tbl, currow, curcol);

			if (p && p->label)
			    center(currow, curcol, currow, curcol);
			else
			    error("Nothing to center");
			break;
		    }
		case 'e':
		    if (!locked_cell(currow, curcol)) {
			struct ent *p = lookat(currow, curcol);

			/* set mark 0 */
			savedrow[27] = currow;
			savedcol[27] = curcol;
			savedstrow[27] = strow;
			savedstcol[27] = stcol;

			editv(currow, curcol);
			if (!(p->flags & IS_VALID)) {
			    linelim = strlen(line);
			    insert_mode();
			} else
			    edit_mode();
		    }
		    break;
		case 'E':
		    if (!locked_cell(currow, curcol)) {
			/* set mark 0 */
			savedrow[27] = currow;
			savedcol[27] = curcol;
			savedstrow[27] = strow;
			savedstcol[27] = stcol;

			edits(currow, curcol);
			edit_mode();
		    }
		    break;
		case 'f':
		    formatcol(arg);
		    break;
		case 'F': {
		    register struct ent *p = *ATBL(tbl, currow, curcol);
		    if (p && p->format) {
			snprintf(line, sizeof line, "fmt [format] %s \"%s",
				v_name(currow, curcol), p->format);
			edit_mode();
			linelim = strlen(line) - 1;
		    } else {
			snprintf(line, sizeof line, "fmt [format] %s \"",
				   v_name(currow, curcol));
			insert_mode();
			linelim = strlen(line);
		    }
		    break;
		}
		case 'C': {
		    if (braille) {
			braillealt ^= 1;
			break;
		    }
		    error("Color number to set (1-8)?");
		    if ((c=nmgetch()) == ESC || c == ctl('g')) {
			CLEAR_LINE;
			break;
		    }
		    if ((c -= ('1' - 1)) < 1 || c > 8) {
			error("Invalid color number.");
			break;
		    }
		    CLEAR_LINE;
		    snprintf(line, sizeof line, "color %d = ", c);
		    linelim = strlen(line);
		    if (cpairs[c-1] && cpairs[c-1]->expr) {
			decompile(cpairs[c-1]->expr, 0);
			line[linelim] = '\0';
			edit_mode();
		    } else {
			insert_mode();
		    }
		    break;
		}
#ifdef KEY_FIND
		case KEY_FIND:
#endif
		case 'g':
		    snprintf(line, sizeof line, "goto [v] ");
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'n':
		    go_last(0);
		    break;
		case 'N':
		    go_last(1);
		    break;
		case 'P':
		    snprintf(line, sizeof line, "put [\"dest\" range] \"");

/* See the comments under "case 'W':" below for an explanation of the
 * logic here.
 */
		    curfile[strlen(curfile) + 1] = '\0';
		    if (strrchr(curfile, '.') != NULL) {
			size_t l;
			if (!strcmp((strrchr(curfile, '.')), ".sc")) {
			    *strrchr(curfile, '.') = '\0';
			    l = strlen(curfile) + 3;
			    strlcpy(curfile + l, ".\0", sizeof(curfile) - l);
			} else if (scext != NULL &&
				!strcmp((strrchr(curfile, '.') + 1), scext)) {
			    *strrchr(curfile, '.') = '\0';
			    l = strlen(curfile) + strlen(scext) + 1;
			    strlcpy(curfile + l, ".\0", sizeof(curfile) - l);
			}
		    }
		    if (*curfile)
			error("Default path is \"%s.%s\"", curfile,
				scext == NULL ? "sc" : scext);
		    c = *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1));
		    *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1)) = '\0';
		    curfile[strlen(curfile)] = c;
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'M':
		    snprintf(line, sizeof line, "merge [\"source\"] \"");
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'R':
		    if (mdir)
			snprintf(line, sizeof line, "merge [\"macro_file\"] \"%s", mdir);
		    else
			snprintf(line, sizeof line, "merge [\"macro_file\"] \"");
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'D':
		    (void) snprintf(line, sizeof line, "mdir [\"macro_directory\"] \"");
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'A':
		    if (autorun)
			snprintf(line, sizeof line,"autorun [\"macro_file\"] \"%s", autorun);
		    else
			snprintf(line, sizeof line, "autorun [\"macro_file\"] \"");
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'G':
		    snprintf(line, sizeof line, "get [\"source\"] \"");
		    if (*curfile)
			error("Default file is \"%s\"", curfile);
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'W':
		    snprintf(line, sizeof line, "write [\"dest\" range] \"");

/* First, append an extra null byte to curfile.  Then, if curfile ends in
 * ".sc" (or '.' followed by the string in scext), move the '.' to the
 * end and replace it with a null byte.  This results in two consecutive
 * null-terminated strings, the first being curfile with the ".sc" (or '.'
 * and scext) removed, if present, and the second being either "sc." (or
 * scext and '.') or "", depending on whether the ".sc" (or '.' and scext)
 * was present or not.
 */
		    curfile[strlen(curfile) + 1] = '\0';
		    if (strrchr(curfile, '.') != NULL) {
			size_t l;
			if (!strcmp((strrchr(curfile, '.')), ".sc")) {
			    *strrchr(curfile, '.') = '\0';
			    l = strlen(curfile) + 3;
			    strlcpy(curfile + l, ".\0", sizeof(curfile) - l);
			} else if (scext != NULL &&
				!strcmp((strrchr(curfile, '.') + 1), scext)) {
			    *strrchr(curfile, '.') = '\0';
			    l = strlen(curfile) + strlen(scext) + 1;
			    strlcpy(curfile + l, ".\0", sizeof(curfile) - l);
			}
		    }

/* Now append ".asc" (or '.' and the value of ascext) to the possibly
 * truncated curfile.
 */
		    if (*curfile)
			error("Default file is \"%s.%s\"", curfile,
				ascext == NULL ? "asc" : ascext);

/* Now swap the '.' and null bytes again.  If there is no '.', swap a
 * null byte with itself.  This may seem convoluted, but it works well,
 * and obviates the need for a 1024 byte temporary buffer. - CRM
 */
		    c = *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1));
		    *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1)) = '\0';
		    curfile[strlen(curfile)] = c;
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'S':	/* set options */
		    snprintf(line, sizeof line, "set ");
		    error("Options:byrows,bycols,iterations=n,tblstyle=(0|tbl|latex|slatex|tex|frame),<MORE>");
		    linelim = strlen(line);
		    insert_mode();
		    break;
		case 'T':	/* tbl output */
		    snprintf(line, sizeof line, "tbl [\"dest\" range] \"");

/* See the comments under "case 'W':" above for an explanation of the
 * logic here.
 */
		    curfile[strlen(curfile) + 1] = '\0';
		    if (strrchr(curfile, '.') != NULL) {
			size_t l;
			if (!strcmp((strrchr(curfile, '.')), ".sc")) {
			    *strrchr(curfile, '.') = '\0';
			    l = strlen(curfile) + 3;
			    strlcpy(curfile + l, ".\0", sizeof(curfile) - l);
			} else if (scext != NULL &&
				!strcmp((strrchr(curfile, '.') + 1), scext)) {
			    *strrchr(curfile, '.') = '\0';
			    l = strlen(curfile) + strlen(scext) + 1;
			    strlcpy(curfile + l, ".\0", sizeof(curfile) - 1);
			}
		    }
		    if (*curfile && tbl_style == 0) {
			error("Default file is \"%s.%s\"", curfile,
				tbl0ext == NULL ? "cln" : tbl0ext);
		    } else if (*curfile && tbl_style == TBL) {
			error("Default file is \"%s.%s\"", curfile,
				tblext == NULL ? "tbl" : tblext);
		    } else if (*curfile && tbl_style == LATEX) {
			error("Default file is \"%s.%s\"", curfile,
				latexext == NULL ? "lat" : latexext);
		    } else if (*curfile && tbl_style == SLATEX) {
			error("Default file is \"%s.%s\"", curfile,
				slatexext == NULL ? "stx" : slatexext);
		    } else if (*curfile && tbl_style == TEX) {
			error("Default file is \"%s.%s\"", curfile,
				texext == NULL ? "tex" : texext);
		    }
		    c = *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1));
		    *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1)) = '\0';
		    curfile[strlen(curfile)] = c;
		    linelim = strlen(line);
		    insert_mode();
		    break;
#ifdef KEY_DC
		case KEY_DC:
#endif
		case 'x':
		    if (calc_order == BYROWS)
			eraser(lookat(currow, curcol),
				lookat(currow, curcol + arg - 1));
		    else
			eraser(lookat(currow, curcol),
				lookat(currow + arg - 1, curcol));
		    break;
		case 'Q':
		case 'q':
		    running = 0;
		    break;
		case KEY_LEFT:
		case 'h':
		    backcol(arg);
		    break;
		case KEY_DOWN:
		case 'j':
		    forwrow(arg);
		    break;
		case KEY_UP:
		case 'k':
		    backrow(arg);
		    break;
		case 'H':
			backcol(curcol - stcol + 2);
			break;
#ifdef KEY_NPAGE
		case KEY_NPAGE:			/* next page */
#endif
		case 'J':
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    forwrow(arg * ps);
		    strow = strow + (arg * ps);
		    FullUpdate++;
		    }
		    break;
#ifdef	KEY_PPAGE
		case KEY_PPAGE:			/* previous page */
#endif
		case 'K':
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    backrow(arg * ps);
		    strow = strow - (arg * ps);
		    if (strow < 0) strow = 0;
		    FullUpdate++;
		    }
		    break;
#ifdef KEY_HOME
		case KEY_HOME:
		    gohome();
		    break;
#endif
		case 'L':
		    forwcol(lcols - (curcol - stcol) + 1);
		    break;
		case KEY_RIGHT:
		case ' ':
		case 'l':
		    forwcol(arg);
		    break;
		case 'm':
		    markcell();
		    break;
		case 'c':
		    error("Copy marked cell:");
		    if ((c = nmgetch()) == ESC || c == ctl('g')) {
			CLEAR_LINE;
			break;
		    }
		    if (c == '.') {
			copy(NULL, NULL, lookat(currow, curcol), NULL);
			snprintf(line, sizeof line, "copy [dest_range src_range] ");
			linelim = strlen(line);
			insert_mode();
			startshow();
			break;
		    }
		    if (c == '`' || c == '\'')
			c = 0;
		    else if (!(((c -= ('a' - 1)) > 0 && c < 27) ||
			    ((c += ('a' - '0' + 26)) > 26 && c < 37))) {
			error("Invalid mark (must be a-z, 0-9, ` or \')");
			break;
		    }
		    if (savedrow[c] == -1) {
			error("Mark not set");
			break;
		    }
		    CLEAR_LINE;
		    {
			struct ent *p = *ATBL(tbl, savedrow[c], savedcol[c]);
			int c1;
			struct ent *n;

			for (c1 = curcol; arg-- && c1 < maxcols; c1++) {
			    if ((n = *ATBL(tbl, currow, c1))) {
				if (n->flags & IS_LOCKED)
				    continue;
				if (!p) {
				    (void) clearent(n);
				    continue;
				}
			    } else {
				if (!p) break;
				n = lookat(currow, c1);
			    }
			    copyent(n, p, currow - savedrow[c],
				    c1 - savedcol[c], 0, 0, maxrow, maxcol, 0);
			    n->flags |= IS_CHANGED;
			}

			FullUpdate++;
			modflg++;
			break;
		    }
		case '`':
		case '\'':
			dotick(c);
			break;
		case '*':
		    {
			register struct ent *p;

			error("Note: Add/Delete/Show/*(go to note)?");
			if ((c = nmgetch()) == ESC || c == ctl('g')) {
			    CLEAR_LINE;
			    break;
			}
			if (c == 'a' || c == 'A') {
			    snprintf(line, sizeof line, "addnote [target range] %s ", 
				    v_name(currow, curcol));
			    linelim = strlen(line);
			    insert_mode();
			    write_line(ctl('v'));
			    CLEAR_LINE;
			    FullUpdate++;
			    break;
			}
			if (c == 'd' || c == 'D') {
			    p = lookat(currow, curcol);
			    p->nrow = p->ncol = -1;
			    p->flags |= IS_CHANGED;
			    CLEAR_LINE;
			    modflg++;
			    FullUpdate++;
			    break;
			}
			if (c == 's' || c == 'S') {
			    FullUpdate++;
			    shownote = 1;
			    clearok(stdscr,1);
			    error("Highlighted cells have attached notes.");
			    break;
			}
			if (c == '*') {
			    gotonote();
			    CLEAR_LINE;
			    break;
			}
			error("Invalid command");
			break;
		    }
		case 'z':
		    switch (c = nmgetch()) {
			case ctl('m'):
			    strow = currow;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			case '.':
			    strow = -1;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			case '|':
			    stcol = -1;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			case 'c':
			    /* Force centering of current cell (or range, if
			     * we've just jumped to a new range with the goto
			     * command).
			     */
			    strow = -1;
			    stcol = -1;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			default:
			    break;
		    }
		    break;
#ifdef KEY_RESIZE
		case KEY_RESIZE:
#ifndef	SIGWINCH
		    winchg();
#endif
		    break;
#endif
#ifdef NCURSES_MOUSE_VERSION
		case KEY_MOUSE:
		    if (getmouse(&mevent) != OK)
			break;
		    if (mevent.bstate & BUTTON1_CLICKED) {
			mouse_sel_cell(0);
			update(0);
		    } else if (mevent.bstate & BUTTON1_PRESSED) {
			mouse_sel_cell(1);
		    } else if (mevent.bstate & BUTTON1_RELEASED) {
			if (!mouse_sel_cell(2))
			    update(0);
		    }
# if NCURSES_MOUSE_VERSION >= 2
		    else if (mevent.bstate & BUTTON4_PRESSED) {
			scroll_up(1);
			FullUpdate++;
			update(0);
		    } else if (mevent.bstate & BUTTON5_PRESSED) {
			scroll_down();
			FullUpdate++;
			update(0);
		    }
# endif
		    break;
#endif
		default:
		    if ((toascii(c)) != c) {
			error ("Weird character, decimal %d\n",
				(int) c);
		    } else {
			    error ("No such command (%c)", c);
		    }
		    break;
	    }
	edistate = nedistate;
	arg = narg;
    }				/* while (running) */
    inloop = modcheck(" before exiting");
    }				/*  while (inloop) */
    stopdisp();
    write_hist();
#ifdef VMS	/* Until VMS "fixes" exit we should say 1 here */
    exit (1);
#else
    exit (0);
#endif
    /*NOTREACHED*/
}

/* set the calculation order */
void
setorder(int i)
{
	if ((i == BYROWS) || (i == BYCOLS))
	    calc_order = i;
}

void
setauto(int i)
{
	autocalc = i;
}

void
signals(void)
{
    (void) signal(SIGINT, doquit);
#if !defined(MSDOS)
    (void) signal(SIGQUIT, dump_me);
    (void) signal(SIGPIPE, nopipe);
    (void) signal(SIGALRM, time_out);
#ifndef __DJGPP__
    (void) signal(SIGBUS, doquit);
#endif
#endif
    (void) signal(SIGTERM, doquit);
    (void) signal(SIGFPE, doquit);
#ifdef	SIGWINCH
    (void) signal(SIGWINCH, winchg);
#endif
}

#ifdef SIGVOID
void
#else
int
#endif
nopipe(int i)
{
    (void)i;
    brokenpipe = TRUE;
}

#ifdef SIGVOID
void
#else
int
#endif
winchg(int i)
{
    (void)i;
    stopdisp();
    startdisp();
    /*
     * I'm not sure why a refresh() needs to be done both before and after
     * the clearok() and update(), but without doing it this way, a screen
     * (or window) that grows bigger will leave the added space blank. - CRM
     */
    refresh();
    FullUpdate++;
    (void) clearok(stdscr, TRUE);
    update(1);
    refresh();
#ifdef	SIGWINCH
    (void) signal(SIGWINCH, winchg);
#endif
}

#ifdef SIGVOID
void
#else
int
#endif
doquit(int i)
{
    (void)i;
    if (usecurses) {
	diesave();
	stopdisp();
    }
    write_hist();
    exit (1);
}

#ifdef SIGVOID
void
#else
int
#endif
dump_me(int i)
{
    (void)i;
    if (usecurses)
	diesave();
    deraw(1);
    abort();
}

/* try to save the current spreadsheet if we can */
void
diesave(void) {
    char	path[PATHLEN];

    if (modcheck(" before Spreadsheet dies") == 1)
    {	snprintf(path, sizeof path, "~/%s", SAVENAME);
	if (writefile(path, 0, 0, maxrow, maxcol) < 0)
	{
	    snprintf(path, sizeof path, "/tmp/%s", SAVENAME);
	    if (writefile(path, 0, 0, maxrow, maxcol) < 0)
		error("Couldn't save current spreadsheet, Sorry");
	}
    }
}

/* check if tbl was modified and ask to save */
int
modcheck(char *endstr)
{
    if (modflg && curfile[0]) {
	int	yn_ans;
	char	lin[32+strlen(curfile)+strlen(endstr)];

	snprintf(lin, sizeof lin, "File \"%s\" is modified, save%s? ",curfile,endstr);
	if ((yn_ans = yn_ask(lin)) < 0)
		return(1);
	else
	if (yn_ans == 1) {
	    if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
 		return (1);
	}
    } else if (modflg) {
	int	yn_ans;

	if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
		return(1);
	else
		return(yn_ans);
    }
    return(0);
}

/* Returns 1 if cell is locked, 0 otherwise */
int
locked_cell(int r, int c)
{
    struct ent *p = *ATBL(tbl, r, c);
    if (p && (p->flags & IS_LOCKED)) {
	error("Cell %s%d is locked", coltoa(c), r) ;
	return(1);
    }
    return(0);
}

/* Check if area contains locked cells */
int
any_locked_cells(int r1, int c1, int r2, int c2)
{
    int r, c;
    struct ent *p ;

    for (r=r1; r<=r2; r++)
	for (c=c1; c<=c2; c++) {
	    p = *ATBL(tbl, r, c);
	    if (p && (p->flags&IS_LOCKED))
		return(1);
	}
    return(0);
}

static void
settcattr(void) {
#ifdef VDSUSP
	static struct termios tty;
# ifdef _PC_VDISABLE
	static long vdis;

	if ((vdis = fpathconf(STDIN_FILENO, _PC_VDISABLE)) == -1) {
		fprintf(stderr,
		    "fpathconf(STDIN, _PC_VDISABLE) failed: %s\n",
		    strerror(errno));
		vdis = 255;
	}
# else
#  define vdis 255
# endif
	if (tcgetattr(STDIN_FILENO, &tty) == -1) {
		fprintf(stderr, "tcgetattr STDIN failed: %s\n",
		    strerror(errno));
		return;
	}
	tty.c_cc[VDSUSP] = vdis;
	if (tcsetattr(STDIN_FILENO, TCSADRAIN, &tty) == -1) {
		fprintf(stderr, "tcsetattr STDIN failed: %s\n",
		    strerror(errno));
		return;
	}
#endif /* VDSUSP */
}

void
mouseon(void) {
#ifdef NCURSES_MOUSE_VERSION
	mousemask(BUTTON1_CLICKED
# if NCURSES_MOUSE_VERSION >= 2
	    | BUTTON4_PRESSED | BUTTON5_PRESSED
# endif
	    , NULL);
# if NCURSES_MOUSE_VERSION < 2
	error("Warning: NCURSES_MOUSE_VERSION < 2");
# endif
#else
	error("Error: NCURSES_MOUSE_VERSION undefined");
#endif
}

void
mouseoff(void) {
#if NCURSES_MOUSE_VERSION >= 2
	mousemask(0, NULL);
#endif
}

#ifdef NCURSES_MOUSE_VERSION
int
mouse_sel_cell(int mode) { /* 0: set, 1: save, 2: cmp and set */
	int i, y, x, tx, ty;
	static int x1, y1;
	if ((y = mevent.y - RESROW) < 0 || (x = mevent.x - rescol) < 0)
		return 1;
	for (ty = strow, i = y; ; ty++) {
		if (row_hidden[ty])
			continue;
		if (--i < 0)
			break;
	}
	for (tx = stcol, i = x; ; tx++) {
		if (col_hidden[tx])
			continue;
		if ((i -= fwidth[tx]) < 0)
			break;
	}
	switch (mode) {
	case 1:
		y1 = ty; x1 = tx;
		break;
	case 2:
		if (y1 != ty || x1 != tx)
			break;
	default:
		currow = ty; curcol = tx;
		return 0;
	}
	return 1;
}
#endif

static void
scroll_down(void) {
	strow++;
	while (row_hidden[strow])
		strow++;
	if (currow < strow)
		currow = strow;
}

static void
scroll_up(int x) {
	if (strow)
		strow--;
	while (strow && row_hidden[strow])
		strow--;
	forwrow(x);
	if (currow >= lastendrow)
		backrow(1);
	backrow(x);
}
