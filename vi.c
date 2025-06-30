/*	SC	A Spreadsheet Calculator
 *
 *	One line vi emulation
 *	$Revision: 7.16 $
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#if defined(REGCOMP)
#include <regex.h>
#endif
#include "compat.h"
#include "sc.h"
#if defined(RE_COMP)
extern char *re_comp(char *s);
extern char *re_exec(char *s);
#endif
#if defined(REGCMP)
char *regcmp();
char *regex();
#endif

#define istext(a) (isalnum(a) || ((a) == '_'))

static void append_line(void);
static void back_hist(void);
static int  back_line(int arg);
static int  back_word(int arg, int big_word);
static void back_space(void);
static void change_case(int arg);
static void col_0(void);
static void cr_line(void);
static void del_in_line(int arg, int back_null);
static void del_to_end(void);
static void doabbrev(void);
static void dogoto(void);
static void dotab(void);
static void dotcmd(void);
static int  find_char(int arg, int n);
static void for_hist(void);
static int  for_line(int arg, int stop_null);
static int  for_word(int arg, int end_word, int big_word, int stop_null);
static int  istart;
static void last_col(void);
static void match_paren(void);
static void readhistfile(FILE *fp);
static void rep_char(void);
static void replace_in_line(int c);
static void replace_mode(void);
static void restore_it(void);
static void savedot(int c);
static void save_hist(void);
static void search_again(bool reverse);
static void search_hist(void);
static void search_mode(char sind);
static void stop_edit(void);
static int  to_char(int arg, int n);
static void u_save(int c);
static void yank_cmd(int delete, int change);
static void yank_chars(register int first, register int last, int delete);
static int get_motion(int);
int vigetch(void);
#ifdef NCURSES_MOUSE_VERSION
void mouse_set_pos(int);
#endif

extern int framerows;		/* Rows in current frame */
extern char mode_ind;		/* Mode indicator */
extern char search_ind;		/* Search indicator */
extern int lcols;		/* Spreadsheet Column the cursor was in last */
static char	*completethis = NULL;
static int	search_dir;		/* Search direction:  forward = 0; back = 1 */
char histfile[PATHLEN] = "~/.sc_history";

/* values for mode below */

#define INSERT_MODE	0	/* Insert mode */
#define EDIT_MODE       1	/* Edit mode */
#define REP_MODE        2	/* Replace mode */
#define SEARCH_MODE	3	/* Get arguments for '/' command */
#define NAVIGATE_MODE	4	/* Navigate the spreadsheet while editing
					a line */

#define	DOTLEN		200

static int mode = INSERT_MODE;
static struct	hist {
	unsigned int	len;
	char		*histline;
} history[HISTLEN + 1];

static int	histp = 0;
static int	lasthist = 0;
static int	endhist = -1;
static int	histsessionstart = 0;
static int	histsessionnew = 0;

#ifdef REGCOMP
static regex_t	preg;
static regex_t	*last_search = NULL;
static int	errcode;
#else
#ifndef RE_COMP
static char	*last_search = NULL;
#endif
#endif
static char	*undo_line = NULL;
static unsigned	undolen = 0;
static int	undo_lim;
static char	dotb[DOTLEN];
static int	doti = 0;
static int	do_dot = 0;
static int	nosavedot = 1;
static int	dotarg = 1;
static char	putbuf[FBUFLEN];
static int	findfunc = '\0';
static int	findchar = 1;
static int	finddir = 0;

void
write_line(int c)
{
    struct frange *fr;
    struct crange *cr;

    CLEAR_LINE;
    if (c != ctl('i')) completethis = NULL;
    if (mode == EDIT_MODE) {
	nosavedot = 0;
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	linelim = back_line(arg);		break;
	case (ctl('i')):	dotab();				break;
	case (ctl('m')):	if (search_ind != ' ')
				    search_hist();
				else
				    cr_line();
			        break;
	case 'v':
	case (ctl('v')):	toggle_navigate_mode();			break;
	case ESC:	stop_edit();					break;
	case '+':	for_hist();					break;
	case '-':	back_hist();					break;
	case KEY_END:
	case (ctl('e')):
	case '$':	last_col();					break;
	case '.':	dotcmd();					break;
	case '!':	doshell();					break;
	case ';':	if (findfunc)
			    ungetch(findchar);
			else
			    break;
			findchar = 0;
			if (findfunc == 'f')
			    linelim = find_char(arg, finddir);
			else
			    linelim = to_char(arg, finddir);
									break;
	case ',':	if (findfunc)
			    ungetch(findchar);
			else
			    break;
			findchar = 0;
			if (findfunc == 'f')
			    linelim = find_char(arg, -(finddir));
			else
			    linelim = to_char(arg, -(finddir));
									break;
	case '~':	u_save(c); change_case(arg);			break;
	case '%':	match_paren();					break;
#ifdef KEY_FIND
	case KEY_FIND:
#endif
	case '?':
	case '/':	search_mode(c);					break;
	case KEY_HOME:
	case (ctl('a')):
	case '0':	col_0();					break;
	case 'A':	u_save(c); last_col(); append_line();		break;
	case 'B':	linelim = back_word(arg, 1);			break;
	case 'C':	u_save(c); del_to_end(); append_line();		break;
	case 'D':	u_save(c); del_to_end();			break;
	case 'E':	linelim = for_word(arg, 1, 1, 0);		break;
	case 'F':	linelim = find_char(arg, -1);			break;
	case 'G':	if (histp > 0) histp = lasthist; for_hist();	break;
	case 'I':	u_save(c); col_0(); insert_mode();		break;
	case 'N':	search_again(true);				break;
	case 'P':	u_save(c);
			ins_string(putbuf);
			linelim = back_line(1);				break;
	case 'R':	u_save(c); replace_mode();			break;
	case 'T':	linelim = to_char(arg, -1);			break;
	case 'W':	linelim = for_word(arg, 0, 1, 0);		break;
	case 'X':	u_save(c); back_space();			break;
	case 'Y':	yank_chars(linelim, strlen(line), 0);		break;
	case 'a':	u_save(c); append_line();			break;
	case 'b':	linelim = back_word(arg, 0);			break;
	case 'c':	u_save(c); yank_cmd(1, 1); insert_mode();	break;
	case 'd':	u_save(c); yank_cmd(1, 0);			break;
	case 'e':	linelim = for_word(arg, 1, 0, 0);		break;
	case 'f':	linelim = find_char(arg, 1);			break;
	case KEY_LEFT:
	case (ctl('b')):
	case 'h':	linelim = back_line(arg);			break;
	case KEY_IC:
	case 'i':	u_save(c); insert_mode();			break;
	case KEY_DOWN:
	case 'j':	for_hist();					break;
	case KEY_UP:
	case 'k':	back_hist();					break;
	case KEY_RIGHT:
	case (ctl('f')):
	case ' ':
	case 'l':	linelim = for_line(arg, 0);			break;
	case 'n':	search_again(false);				break;
	case 'p':	u_save(c);
			linelim = for_line(1, 1);
			ins_string(putbuf);
			linelim = back_line(1);				break;
	case 'q':	stop_edit();					break;
	case 'r':	u_save(c); rep_char();				break;
	case 's':	u_save(c); del_in_line(arg, 0); insert_mode();	break;
	case 't':	linelim = to_char(arg, 1);			break;
	case 'u':	restore_it();					break;
	case 'w':	linelim = for_word(arg, 0, 0, 0);		break;
	case KEY_DC:
	case 'x':	u_save(c); del_in_line(arg, 1);			break;
	case 'y':	yank_cmd(0, 0);					break;
#ifdef NCURSES_MOUSE_VERSION
	case KEY_MOUSE:
	    if (getmouse(&mevent) != OK || mevent.y)
		break;
	    if (mevent.bstate & BUTTON1_CLICKED)
		mouse_set_pos(0);
	    else if (mevent.bstate & BUTTON1_PRESSED)
		mouse_set_pos(1);
	    else if (mevent.bstate & BUTTON1_RELEASED)
		mouse_set_pos(2);
	    break;
#endif
	default:
	    break;
	}
    } else if (mode == INSERT_MODE) { 
	if (c == (ctl('m')))
	    savedot(ESC);
	else
	    savedot(c);
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	back_space();				break;
	case (ctl('i')):	dotab();				break;
	case (ctl('m')):	cr_line();				break;
	case (ctl('v')):	toggle_navigate_mode();			break;
	case KEY_LEFT:
	case (ctl('b')):	if (numeric_field) {
				    if (linelim > 0 &&
				            (size_t)linelim == strlen(line) &&
					    (line[linelim - 1] == '+' ||
					     line[linelim - 1] == '-')) {
					toggle_navigate_mode();
					backcol(1);
				    } else {
					c = craction;
					craction = 0;
					cr_line();
					craction = c;
					backcol(1);
				    }
				} else {
				    linelim = back_line(arg);
				    istart = linelim;
				}   break;
	case KEY_RIGHT:
	case (ctl('f')):	if (numeric_field) {
				    if (linelim > 0 &&
				            (size_t)linelim == strlen(line) &&
					    (line[linelim - 1] == '+' ||
					     line[linelim - 1] == '-')) {
					toggle_navigate_mode();
					forwcol(1);
				    } else {
					c = craction;
					craction = 0;
					cr_line();
					craction = c;
					forwcol(1);
				    }
				} else {
				    linelim = for_line(arg, 1);
				    istart = linelim;
				}   break;
	case KEY_DOWN:
	case (ctl('n')):	if (numeric_field) {
				    if (linelim > 0 &&
				            (size_t)linelim == strlen(line) &&
					    (line[linelim - 1] == '+' ||
					     line[linelim - 1] == '-')) {
					toggle_navigate_mode();
					forwrow(1);
				    } else {
					c = craction;
					craction = 0;
					cr_line();
					craction = c;
					forwrow(1);
				    }
				} else {
				    for_hist();
				    istart = linelim;
				}   break;
	case KEY_UP:
	case (ctl('p')):	if (numeric_field) {
				    if (linelim > 0 &&
				            (size_t)linelim == strlen(line) &&
					    (line[linelim - 1] == '+' ||
					     line[linelim - 1] == '-')) {
					toggle_navigate_mode();
					backrow(1);
				    } else {
					c = craction;
					craction = 0;
					cr_line();
					craction = c;
					backrow(1);
				    }
				} else {
				    back_hist();
				    istart = linelim;
				}   break;
	case KEY_HOME:
	case (ctl('a')):	col_0();				break;
	case KEY_END:
	case (ctl('e')):	last_col();				break;
	case ESC:		ins_in_line(0);
				edit_mode();				break;
	/* '\035' is ^], which expands abbreviations without inserting another
	 * character in the line
	 */
	case '\035':		if (linelim > 0) doabbrev();		break;
#ifdef NCURSES_MOUSE_VERSION
	case KEY_MOUSE:
	    if (getmouse(&mevent) != OK || mevent.y)
		break;
	    if (mevent.bstate & BUTTON1_CLICKED)
		mouse_set_pos(0);
	    else if (mevent.bstate & BUTTON1_PRESSED)
		mouse_set_pos(1);
	    else if (mevent.bstate & BUTTON1_RELEASED)
		mouse_set_pos(2);
	    break;
#endif
	default:		ins_in_line(c);				break;
	}
    } else if (mode == SEARCH_MODE) {
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	back_space();				break;
	case (ctl('m')):	search_hist();				break;
	case ESC:		ins_in_line(0);
				edit_mode();				break;
	/* '\035' is ^], which expands abbreviations without inserting another
	 * character in the line
	 */
	case '\035':		if (linelim > 0) doabbrev();		break;
	default:		ins_in_line(c);				break;
	}
    } else if (mode == REP_MODE) {
	savedot(c);
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	if (linelim >= 0 &&
				  (size_t)linelim > strlen(undo_line))
				    back_space();
				else {
				    linelim = back_line(1);
				    *(line+linelim) = *(undo_line+linelim);
				}					break;
	case (ctl('m')):	cr_line();				break;
	case ESC:		edit_mode();				break;
	default:		replace_in_line(c);			break;
	}
    } else if (mode == NAVIGATE_MODE) {
	switch (c) {
	case '.':
	case ':':
	case (ctl('i')):	if (!showrange) {
				    toggle_navigate_mode();
				    startshow();
				} else if ((size_t)linelim == strlen(line) &&
					(line[linelim - 1] == '+' ||
					line[linelim - 1] == '-' ||
					(line[linelim - 1] == ' ' &&
					 line[linelim - 2] == '='))) {
				    ins_string("@sum(");
				    showdr();
				    ins_in_line(')');
				} else {
				    showdr();
				    ins_in_line(' ');
				} 					break;
	case ' ':		if (showrange) {
				    showdr();
				    ins_in_line(' ');
				    toggle_navigate_mode();
				} else {
				    forwcol(arg);
				} 					break;
	case '+':
	case '-':		if (!showrange) {
				    ins_string(v_name(currow, curcol));
				    ins_in_line(c);
				} else if ((size_t)linelim == strlen(line) &&
					(line[linelim - 1] == '+' ||
					line[linelim - 1] == '-' ||
					(line[linelim - 1] == ' ' &&
					 line[linelim - 2] == '='))) {
				    ins_string("@sum(");
				    showdr();
				    ins_in_line(')');
				    ins_in_line(c);
				    toggle_navigate_mode();
				} else {
				    showdr();
				    ins_in_line(')');
				    ins_in_line(c);
				}					break;
	case (ctl('m')):	if (!showrange) {
				    ins_string(v_name(currow, curcol));
				    toggle_navigate_mode();
				} else {
				    toggle_navigate_mode();
				    cr_line();
				}					break;
	case 'v':	{   /* insert variable value */
				    struct ent *p = *ATBL(tbl, currow, curcol);
				    char temp[100];

				    if (p && p->flags & IS_VALID) {
					snprintf(temp, sizeof temp, "%.*f",
						precision[curcol], p->v);
					ins_string(temp);
					toggle_navigate_mode();
				    }
				}					break;
	case 'c':		if ((cr = find_crange(currow, curcol))) {
				    ins_string(r_name(cr->r_left->row,
					    cr->r_left->col,
					    cr->r_right->row,
					    cr->r_right->col));
				    toggle_navigate_mode();
				    ins_in_line(' ');
				    showrange = 0;
				}					break;
	case 'f':		if ((fr = find_frange(currow, curcol))) {
				    ins_string(r_name(fr->or_left->row,
					    fr->or_left->col,
					    fr->or_right->row,
					    fr->or_right->col));
				    toggle_navigate_mode();
				    ins_in_line(' ');
				    showrange = 0;
				}					break;
	case 'r':		if ((fr = find_frange(currow, curcol))) {
				    ins_string(r_name(fr->ir_left->row,
					    fr->ir_left->col,
					    fr->ir_right->row,
					    fr->ir_right->col));
				    toggle_navigate_mode();
				    ins_in_line(' ');
				    showrange = 0;
				}					break;
	case KEY_LEFT:
	case 'h':		backcol(arg);				break;
	case KEY_RIGHT:
	case 'l':		forwcol(arg);				break;
	case KEY_DOWN:
	case (ctl('n')):
	case 'j':		forwrow(arg);				break;
	case KEY_UP:
	case (ctl('p')):
	case 'k':		backrow(arg);				break;
	case 'q':
	case ctl('g'):
	case (ctl('v')):
	case ESC:		toggle_navigate_mode();
				showrange = 0;				break;
	case 'H':		backcol(curcol - stcol + 2);
									break;
	case KEY_NPAGE:			/* next page */
	case (ctl('f')):
	case 'J':
				{
				int ps;

				ps = pagesize ? pagesize :
				    (LINES - RESROW - framerows)/2;
				forwrow(arg * ps);
				strow = strow + (arg * ps);
				FullUpdate++;
				}
									break;
	case KEY_PPAGE:			/* previous page */
	case (ctl('b')):
	case 'K':
				{
				int ps;

				ps = pagesize ? pagesize :
				    (LINES - RESROW - framerows)/2;
				backrow(arg * ps);
				strow = strow - (arg * ps);
				if (strow < 0) strow = 0;
				FullUpdate++;
				}
									break;
	case 'L':		forwcol(lcols - (curcol - stcol) + 1);	break;
	case 'N':		go_last(1);				break;
	case (ctl('a')):
	case KEY_HOME:		gohome();				break;
	case '0':		leftlimit();				break;
	case '$':		rightlimit();				break;
	case '^':		gototop();				break;
	case '#':		gotobottom();				break;
	case 'o':		if (showrange) {
				    int r = currow;
				    int cc = curcol;

				    currow = showsr;
				    showsr = r;
				    curcol = showsc;
				    showsc = cc;
				    rowsinrange = 1;
				    colsinrange = fwidth[curcol];
				} 					break;
	case 'm':		markcell();				break;
	case '`': case '\'':	dotick(c);				break;
	case '*':		if (nmgetch() == '*') gotonote();	break;
	case 'g':		dogoto();				break;
	case 'n':		go_last(0);				break;
	case 'w':		{
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
	case 'b':		{
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
	case 'C':		if (braille)
				    braillealt ^= 1;
				break;
	}
    }
}

void
edit_mode(void) {
    mode_ind = 'e';
    mode = EDIT_MODE;
    if (linelim < 0)	/* -1 says stop editing, ...so we still aren't */
	return;
    numeric_field = 0;
    linelim = back_line(1);
}

void
insert_mode(void)
{
    mode_ind = 'i';
    mode = INSERT_MODE;
    istart = linelim;
}

static void
search_mode(char sind)
{
    if (search_ind == ' ') {
	/*
	 * This back and forth movement through the history is just a quick
	 * way to force the current command to be saved in history[0].histline,
	 * allocating space for it if necessary.  The command will be copied
	 * back into line by search_hist() before the search is done. - CRM
	 */
	back_hist();
	for_hist();
	line[0] = '\0';
	linelim = 0;
	mode_ind = 'i';
	search_ind = sind;
	search_dir = sind == '/' ? 1 : 0;
	mode = SEARCH_MODE;
	istart = linelim;
    }
}

static void
replace_mode(void) {
    mode_ind = 'R';
    mode = REP_MODE;
}

void
toggle_navigate_mode(void)
{
    static char prev_mode = NAVIGATE_MODE;
    int limtmp;

    switch (prev_mode) {
	case INSERT_MODE:
				if (mode == NAVIGATE_MODE) {
				    prev_mode = NAVIGATE_MODE;
				    insert_mode();
				    break;
				} else
				    return;
	case EDIT_MODE:
				if (mode == NAVIGATE_MODE) {
				    prev_mode = NAVIGATE_MODE;
				    limtmp = linelim;
				    edit_mode();
				    linelim = limtmp;
				    break;
				} else
				    return;
	case NAVIGATE_MODE:
				prev_mode = mode;
				mode_ind = 'v';
				mode = NAVIGATE_MODE;
				break;
	default:
				prev_mode = NAVIGATE_MODE;
				break;
    }
}

void
dotab(void) {
    static struct range *firstmatch;
    static struct range *lastmatch;
    static struct range *nextmatch;
    int len;

    if ((linelim > 0 && isalnum((int)line[linelim-1])) || line[linelim-1] == '_' ||
	    (completethis && line[linelim-1] == ' ')) {
	if (!completethis) {
	    for (completethis = line + linelim - 1; isalnum((int)*completethis) ||
		    *completethis == '_'; completethis--) /* */;
	    completethis++;
	    len = line + linelim - completethis;
	    if (!find_range(completethis, -len, NULL, NULL, &lastmatch)) {
		firstmatch = lastmatch;
		while (firstmatch->r_next &&
			!strncmp(completethis, firstmatch->r_next->r_name, len))
		    firstmatch = firstmatch->r_next;
		nextmatch = firstmatch;
	    } else
		nextmatch = NULL;
	}
	if (nextmatch) {
	    len = line + linelim - completethis;
	    memmove(completethis, line + linelim, strlen(line) - linelim + 1);
	    linelim -= len;
	    ins_string(nextmatch->r_name);
	    if (*(completethis - 1) == ' ' && *(line + linelim) != ' ')
		ins_in_line(' ');
	    if (nextmatch == lastmatch)
		nextmatch = firstmatch;
	    else
		nextmatch = nextmatch->r_prev;
	}
    } else
	startshow();
}

/* show the current range (see ^I), we are moving around to define a range */
void
startshow(void)
{
    showrange = 1;
    showsr = currow;
    showsc = curcol;
    toggle_navigate_mode();
}

/* insert the range we defined by moving around the screen, see startshow() */

void
showdr(void)
{
    int			minsr, minsc, maxsr, maxsc;
    char		r[12];
    struct frange	*fr = find_frange(currow, curcol);

    if (showrange == SHOWROWS) {
	minsr = showsr < currow ? showsr : currow;
	minsc = fr ? fr->or_left->col : 0;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = fr ? fr->or_right->col : maxcols;

	toggle_navigate_mode();
	snprintf(r, sizeof r, "%d:%d", minsr, maxsr);
	ins_string(r);
    } else if (showrange == SHOWCOLS) {
	minsr = 0;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = maxrows;
	maxsc = showsc > curcol ? showsc : curcol;

	strlcpy(r, coltoa(minsc), sizeof r);
	strlcat(r, ":", sizeof r);
	strlcat(r, coltoa(maxsc), sizeof r);

	toggle_navigate_mode();
	ins_string(r);
    } else {
	minsr = showsr < currow ? showsr : currow;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = showsc > curcol ? showsc : curcol;

	toggle_navigate_mode();
	ins_string(r_name(minsr, minsc, maxsr, maxsc));
    }
    showrange = 0;
}

/* dot command functions.  Saves info so we can redo on a '.' command */

static void
savedot(int c)
{
    if (do_dot || nosavedot || (c == '\n'))
	return;

    if (doti == 0) dotarg = arg;
    if (doti < DOTLEN-1) {
	if (c > 255) {
	    if (doti < DOTLEN-2) {
		dotb[doti++] = c/256;
		c %= 256;
	    } else
		return;
	}
	dotb[doti++] = c;
	dotb[doti] = '\0';
    }
}

static void
dotcmd(void) {
    static int dotcalled = 0;
    int c;

    if (dotcalled)	/* stop recursive calling of dotcmd() */
	return;
    do_dot = 1;
    doti = 0;
    if (arg == 1)
	arg = dotarg;
    else
	dotarg = arg;
    while (dotb[doti] != '\0') {
	if ((c = dotb[doti++]) < 4)
	    c = c * 256 + dotb[doti++];
	dotcalled = 1;
	write_line(c);
    }
    do_dot = 0;
    doti = 0;
    dotcalled = 0;
}

int
vigetch(void) {
    int c;

    if (do_dot) {
	if (dotb[doti] != '\0') {
	    return (dotb[doti++]);
	} else {
	    do_dot = 0;
	    doti = 0;
	    return (nmgetch());
	}
    }
    update(1);
    c = nmgetch();
    return (c);
}

/* saves the current line for possible use by an undo cmd */
static void
u_save(int c)
{
    if (strlen(line)+1 > undolen) {
    	undolen = strlen(line)+40;

	undo_line = scxrealloc(undo_line, undolen);
    }
    strlcpy(undo_line, line, undolen);

    undo_lim = linelim;

    /* reset dot command if not processing it. */

    if (!do_dot) {
        doti = 0;
	savedot(c);
    }
}

/* Restores the current line saved by u_save() */
static void
restore_it(void) {
    static char		*tempc = NULL;
    static unsigned	templen = 0;
    int			tempi;

    if ((undo_line == NULL) || (*undo_line == '\0')) 
	return;

    if (strlen(line)+1 > templen) {
    	templen = strlen(line) + 40;
	tempc = scxrealloc(tempc, templen);
    }

    strlcpy(tempc, line, templen);
    tempi = linelim;
    strlcpy(line, undo_line, sizeof line);
    linelim = undo_lim;
    strlcpy(undo_line, tempc, undolen);
    undo_lim = tempi;
}

/* This command stops the editing process. */
static void
stop_edit(void) {
    if (search_ind != ' ') {
	search_ind = ' ';
	strlcpy(line, history[0].histline, sizeof line);
	write_line('G');
    } else {
	showrange = 0;
	numeric_field = 0;
	linelim = -1;
	(void) move(1, 0);
	(void) clrtoeol();
    }
}

/*
 * Motion commands.  Forward motion commands take an argument
 * which, when set, cause the forward motion to continue onto
 * the null at the end of the line instead of stopping at the
 * the last character of the line.
 */
static int
for_line(int a, int stop_null)
{
    ssize_t cpos = linelim;

    if (linelim < 0)
	return (linelim);
    else if (a >= 0 && (size_t)(linelim + a) <= strlen(line))
	cpos += a;
    else
	cpos = strlen(line);

    if (cpos > 0 && (size_t)cpos == strlen(line) && !stop_null)
	return (cpos - 1);
    else
	return (cpos);
}

/* If end_word is non-zero, go to next end-of-word.  Otherwise, go to next
 * beginning-of-word.
 */

static int
for_word(int a, int end_word, int big_word, int stop_null)
{
    register int c;
    ssize_t cpos;

    cpos = linelim;

    while (cpos >= 0 && (size_t)cpos < strlen(line) && a--) {
	if (end_word)
	    cpos++;

	if (line[cpos] == ' ') {
	    while (line[cpos] == ' ')
		cpos++;
	    if (cpos > 0 && line[cpos] == '\0')
		--cpos;
	    if (!end_word)
		continue;
	}

	if (big_word)
	    while ((c = line[cpos]) && c != ' ')
		cpos++;
	else if (istext((int)line[cpos])) {
	    while ((c = line[cpos]) && istext(c)) 
		cpos++;
	} else {
	    while ((c = line[cpos]) && !istext(c) && c != ' ')
		cpos++;
	}

	if (end_word)
	    cpos--;
	else while (line[cpos] == ' ')
	    cpos++;

	if (cpos > 0 && line[cpos] == '\0' && !stop_null) 
	    --cpos;
    }

    return (cpos);
}

static int
back_line(int a)
{
    if (linelim > a)
        return (linelim - a);
    else
	return (0);
}

static int
back_word(int a, int big_word)
{
    register int c;
    register int cpos;

    cpos = linelim;

    while (cpos > 0 && a--) {
	if (line[cpos] == ' ') {
	    /* Skip white space */
	    while (cpos > 0 && line[cpos] == ' ')
		--cpos;
	} else if (cpos > 0 && (line[cpos-1] == ' ' 
			|| ( istext((int)line[cpos]) && !istext((int)line[cpos-1]))
			|| (!istext((int)line[cpos]) &&  istext((int)line[cpos-1])))) {
	    /* Started on the first char of a word - back up to prev. word */
	    --cpos;
	    while (cpos > 0 && line[cpos] == ' ')
		--cpos;
	}

	/* Skip across the word - goes 1 too far */
	if (big_word)
	    while (cpos > 0 && (c = line[cpos]) && c != ' ')
		--cpos;
	else if (istext((int)line[cpos])) {
	    while (cpos > 0 && (c = line[cpos]) && istext(c)) 
		--cpos;
	} else {
	    while (cpos > 0 && (c = line[cpos]) && !istext(c) && c != ' ')
		--cpos;
	}

	/* We are done - fix up the one too far */
	if (cpos > 0 && line[cpos] && line[cpos+1]) 
	    cpos++;
    }

    return(cpos);
}

/* Text manipulation commands */

/* If back_null is set, back up if the deletion leaves you at the null
 * line terminator.  Otherwise, don't.
 */
static void
del_in_line(int a, int back_null)
{
    register int len, i;

    if (linelim >= 0) {
	len = strlen(line);
	if (a > len - linelim)
	    a = len - linelim;
	if (linelim == len && linelim > 0)
	    linelim--;
	strlcpy(putbuf, line + linelim, a);
	putbuf[a] = '\0';
	for (i = linelim; i < len; i++)
	    line[i] = line[i+a];
    }
    if (back_null && linelim > 0 && line[linelim] == '\0')
	--linelim;
}

void
ins_in_line(int c)
{
    int i, len;
    static int inabbr;

    if (c < 256) {
	if (linelim < 0 && c > 0) {
	    *line = '\0';
	    linelim = 0;
	}
	if (!inabbr && linelim > 0 && !(isalpha(c) || isdigit(c) || c == '_')) {
	    inabbr++;
	    doabbrev();
	    inabbr--;
	}
	if (c > 0) {
	    len = strlen(line);
	    for (i = len; i >= linelim; --i)
		line[i+1] = line[i];
	    line[linelim++] = c;
	    line[len+1] = '\0';
	}
    }
}

void
ins_string(char *s)
{
    while (*s)
	ins_in_line(*s++);
}

void
doabbrev(void) {
    int len, pos;
    struct abbrev *a;
    struct abbrev *prev;

    if (istart < 0 || linelim < 2)
	return;

    if (!(isalpha((int)line[linelim - 1]) || isdigit((int)line[linelim - 1]) ||
      line[linelim - 1] == '_') || !(mode == INSERT_MODE ||
      mode == SEARCH_MODE) || istart >= linelim)
	return;

    pos = linelim - 2;
    if (isalpha((int)line[pos]) || isdigit((int)line[pos]) || line[pos] == '_')
    {
	for (; pos >= istart; pos--)
	    if (!(isalpha((int)line[pos]) || isdigit((int)line[pos]) ||
	      line[pos] == '_'))
		break;
    } else if (line[pos] != ' ')
	for (; pos >= istart; pos--)
	    if (isalpha((int)line[pos]) || isdigit((int)line[pos]) || line[pos]
	      == '_' || line[pos] == ' ')
		break;
    pos++;

    if (istart && pos == istart) {
	if (isalpha((int)line[pos]) || isdigit((int)line[pos]) || line[pos] ==
	  '_') {
	    if (isalpha((int)line[--pos]) || isdigit((int)line[pos]) ||
	      line[pos] == '_')
		return;
	} else {
	    if (!(isalpha((int)line[--pos]) || isdigit((int)line[pos]) ||
		    line[pos] == '_' || line[pos] == ' '))
		return;
	}
	pos++;
    }

    len = linelim - pos;
    if (len && (a = find_abbr(line + pos, len, &prev))) {
	if (len > 1 || pos == 0 || line[pos-1] == ' ') {
	    linelim = pos;
	    del_in_line(len, 0);
	    ins_string(a->exp);
	}
    }
}

static void
append_line(void) {
    register int i;

    i = linelim;
    if (i >= 0 && line[i])
	linelim++;
    insert_mode();
}

static void
change_case(int a) {
    if (linelim < 0) {
    	linelim = 0;
	*line = '\0';
    }
    while (a--) {
	if (islower((int)line[linelim]))
	    line[linelim] = toupper((int)line[linelim]);
	else if (isupper((int)line[linelim]))
	    line[linelim] = tolower((int)line[linelim]);
	linelim = for_line(1, 0);
    }
}

static void
rep_char(void) {
    int c;

    if (linelim < 0) {
    	linelim = 0;
	*line = '\0';
    }
    c = vigetch();
    savedot(c);
    if (c < 256 && c!= ESC && c != ctl('g')) {
	if (line[linelim] == '\0')
	    line[linelim+1] = '\0';
	line[linelim] = c;
    }
}

static void
replace_in_line(int c)
{
    register int len;

    if (c < 256) {
	if (linelim < 0) {
	    linelim = 0;
	    *line = '\0';
	}
	len = strlen(line);
	line[linelim++] = c;
	if (linelim > len)
	    line[linelim] = '\0';
    }
}

static void
back_space(void) {
    if (linelim == 0)
	return;

    if (line[linelim] == '\0') {
	linelim = back_line(1);
	del_in_line(1, 1);
	linelim = strlen(line);
    } else {
	linelim = back_line(1);
	del_in_line(1, 1);
    }
    if (linelim < istart)
	istart = linelim;
}

/* Setting change to 1 makes `w' act like `e' so that `cw' will act like
 * `ce', just as in vi.  Setting change to 0 causes `w' to act as usual.
 */

static int
get_motion(int change)
{
    int c;
    int arg2 = 0;

    c = vigetch();
    if (c == '0') {
	savedot(c);
	return (0);
    }
    while (c >= '0' && c <= '9') {
	arg2 = 10 * arg2 + c - '0';
	c = vigetch();
    }
    if (!arg2)
	arg2++;
    arg *= arg2;
    if (!nosavedot) {
	savedot(c);
	dotarg = arg;
    }
    switch (c) {
	case '$':	return (strlen(line));
	case 'b':	return (back_word(arg, 0));
	case 'B':	return (back_word(arg, 1));
	case 'c':	return (change ? -1 : linelim);
	case 'd':	return (!change ? -1 : linelim);
	case 'e':	return (for_word(arg, 1, 0, 1) + 1);
	case 'E':	return (for_word(arg, 1, 1, 1) + 1);
	case 'f':	return ((c = find_char(arg, 1)) == linelim ? c : c + 1);
	case 'F':	return (find_char(arg, -1));
	case 'h':	return (back_line(arg));
	case 'l':	return (for_line(arg, 1));
	case 't':	return ((c = to_char(arg, 1)) == linelim ? c : c + 1);
	case 'T':	return (to_char(arg, -1));
	case 'w':	return (for_word(arg, change, 0, 1) + change);
	case 'W':	return (for_word(arg, change, 1, 1) + change);
	default:	return (linelim);
    }
}

static void
yank_cmd(int delete, int change)
{
    int cpos;

    if ((cpos = get_motion(change)) == -1) {
	cpos++;
	linelim = strlen(line);
    }
    yank_chars(cpos, linelim, delete);
}

static void
yank_chars(register int first, register int last, int delete)
{
    int temp;

    if (first == last)
	return;

    if (last < first) {
	temp = last; last = first; first = temp;
    }

    linelim = first;
    strlcpy(putbuf, line + first, last - first);
    putbuf[last - first] = '\0';
    if (delete)
	memmove(line + first, line + last, strlen(line + last) + 1);
}

static void
del_to_end(void) {
    if (linelim < 0)
	return;
    strlcpy(putbuf, line + linelim, sizeof putbuf);
    line[linelim] = '\0';
    linelim = back_line(1);
}

static void
cr_line(void) {
    struct frange *fr;

    ins_in_line(0);
    insert_mode();
    numeric_field = 0;
    if (linelim == -1) {	/* '\n' alone will put you into insert mode */
    	*line = '\0';		/* unless numeric and craction are both	set */
	linelim = 0;
	if (numeric && craction)
	    cellassign = 1;
	else
	    return;
    }
    save_hist();
    nosavedot = 1;
    linelim = 0;
    (void) yyparse();
    showrange = 0;
    linelim = -1;
    if (cellassign) {
	cellassign = 0;
	switch (craction) {
	    case CRROWS:
		if ((rowlimit >= 0) && (currow >= rowlimit)) {
		    forwcol(1);
		    currow = 0;
		} else {
		    if ((fr = find_frange(currow, curcol))) {
			forwrow(1);
			if (currow > fr->ir_right->row) {
			    backrow(1);
			    if (autowrap) {
				forwcol(1);
				currow = fr->ir_left->row;
				if (row_hidden[currow])
				    forwrow(1);
				if (curcol > fr->ir_right->col) {
				    backcol(1);
				    if (autoinsert)
					insertcol(1, 1);
				    else {
					currow = fr->ir_right->row;
					if (row_hidden[currow])
					    backrow(1);
				    }
				}
			    } else if (autoinsert)
				insertrow(1, 1);
			}
		    } else
			forwrow(1);
		}
		break;
	    case CRCOLS:
		if ((collimit >= 0) && (curcol >= collimit)) {
		    forwrow(1);
		    curcol = 0;
		} else {
		    if ((fr = find_frange(currow, curcol))) {
			forwcol(1);
			if (curcol > fr->ir_right->col) {
			    backcol(1);
			    if (autowrap) {
				forwrow(1);
				curcol = fr->ir_left->col;
				if (col_hidden[curcol])
				    forwcol(1);
				if (currow > fr->ir_right->row) {
				    backrow(1);
				    if (autoinsert)
					insertrow(1, 1);
				    else {
					curcol = fr->ir_right->col;
					if (col_hidden[curcol])
					    backcol(1);
				    }
				}
			    } else if (autoinsert)
				insertcol(1, 1);
			}
		    } else
			forwcol(1);
		}
		break;
	    default:
		break;
	}
    }
}

void
doshell(void)
{
    /*
    *  "! command"  executes command
    *  "!"	forks a shell
    *  "!!" repeats last command
    */
#if VMS || MSDOS
    error("Not implemented on VMS or MS-DOS");
#else /* VMS */
    char *shl;
    int pid, temp;
    char cmd[MAXCMD];
    static char lastcmd[MAXCMD];

    if (!(shl = getenv("SHELL")))
	shl = "/bin/sh";

    deraw(1);
    (void) fputs("! ", stdout);
    (void) fflush(stdout);
    (void) fgets(cmd, MAXCMD, stdin);
    cmd[strlen(cmd) - 1] = '\0';	/* clobber \n */
    if (strcmp(cmd,"!") == 0)		/* repeat? */
	strlcpy(cmd, lastcmd, sizeof cmd);
    else
	strlcpy(lastcmd, cmd, sizeof lastcmd);

    if (modflg) {
	(void) puts("[No write since last change]");
	(void) fflush(stdout);
    }

    if (!(pid = fork())) {
	(void) signal(SIGINT, SIG_DFL);  /* reset */
	if (strlen(cmd))
	    execl(shl, shl, "-c", cmd, (char *)NULL);
	else
	    execl(shl, shl, (char *)NULL);
	exit (-127);
    }

    while (pid != wait(&temp));

    (void) printf("Press any key to continue ");
    fflush(stdout);
    cbreak();
    (void)getch();
    goraw();
    clear();
#endif /* VMS */
}

/* History functions */

static void
save_hist(void) {
    if (!lasthist || strcmp(history[lasthist].histline, line)) {
	if (lasthist < 0)
	    lasthist = 1;
	else
	    lasthist = lasthist % HISTLEN + 1;

	if (lasthist > endhist)
	    endhist = lasthist;

	if (history[lasthist].len < strlen(line) + 1) {
	    history[lasthist].len = strlen(line) + 40;
	    history[lasthist].histline = scxrealloc(history[lasthist].histline,
		    history[lasthist].len);
	}
	strlcpy(history[lasthist].histline, line, history[lasthist].len);
	histsessionnew++;
    }
    if (history[0].histline) {
	scxfree(history[0].histline);
	history[0].histline = NULL;
	history[0].len = 0;
    }
    histp = 0;
}

static void
for_hist(void) {
    if (histp == 0) {
	last_col();
    	return;
    }

    if (histp == lasthist)
	histp = 0;
    else
	histp = histp % endhist + 1;

    if (lasthist >= 0) {
	strlcpy(line, history[histp].histline, sizeof line);
	last_col();
    }
    if (histp) {
	error("History line %d", endhist - lasthist + histp);
    } else {
	CLEAR_LINE;
    }
}

static void
back_hist(void) {
    if (histp == 0) {
	if (history[0].len < strlen(line) + 1) {
	    history[0].len = strlen(line) + 40;
	    history[0].histline = scxrealloc(history[0].histline,
		    history[0].len);
	}
	strlcpy(history[0].histline, line, history[0].len);

	if (lasthist >= 0)
	    histp = lasthist;
    } else if (histp == 1) {
    	if (endhist != lasthist)
	    histp = endhist;
    } else if (histp != ((lasthist + 1) % (endhist + 1)))
	histp--;

    if (lasthist >= 0) {
    	strlcpy(line, history[histp].histline, sizeof line);
	last_col();
    }
    if (histp) {
	error("History line %d", endhist - lasthist + histp);
    } else {
	CLEAR_LINE;
    }
}

static void
search_hist(void) {
#ifdef RECOMP
    char	*tmp;
#endif
#if !defined(REGCOMP) && !defined(RE_COMP) && !defined(REGCMP)
    static	unsigned lastsrchlen = 0;
#endif

    if (linelim < 1) {
	linelim = 0;
	edit_mode();
	return;
    }

#if defined REGCOMP
    if (last_search)
	regfree(last_search);
    else
	last_search = &preg;
    if ((errcode = regcomp(last_search, line, REG_EXTENDED))) {
	char *tmp = scxmalloc(160);
	regerror(errcode, last_search, tmp, sizeof(tmp));
	error("%s", tmp);
	scxfree(tmp);
	return;
    }
#elif defined RE_COMP
    if ((tmp = re_comp(line)) != NULL) {
	error("%s", tmp);
	return;
    }
#elif defined REGCMP
    free(last_search);
    if ((last_search = regcmp(line, NULL)) == NULL) {
	error("Invalid search string");
	return;
    }
#else
    if (strlen(line) + 1 > lastsrchlen) {
    	lastsrchlen = strlen(line) + 40;
	last_search = scxrealloc(last_search, lastsrchlen);
    }
    strlcpy(last_search, line, lastsrchlen);
#endif
    strlcpy(line, history[0].histline, sizeof line);
    search_again(false);
    if (mode != EDIT_MODE) edit_mode();
    search_ind = ' ';
}

static void
search_again(bool reverse)
{
    int prev_match;
    int found_it = 0;
#if !defined(REGCOMP) && !defined(RE_COMP) && !defined(REGCMP)
    char *look_here;
    int do_next;
#endif

#if defined REGCOMP
    if (last_search == NULL)
	return;
#elif !defined(RE_COMP)
    if (last_search == NULL || *last_search == '\0')
	return;
#endif
    prev_match = histp > 0 ? histp : 0;
    CLEAR_LINE;

    do {
	if (lasthist > 0) {
	    if (!(search_dir ^ reverse) && histp != lasthist)
		if (histp <= 0) {
		    histp = ((lasthist + 1) % endhist);
		    strlcpy(line, history[histp].histline, sizeof line);
		} else
		    for_hist();
	    else if ((search_dir ^ reverse) && histp != ((lasthist + 1) % endhist))
		back_hist();
	    else {
		histp = 0;
		strlcpy(line, history[0].histline, sizeof line);
		last_col();
	    }
	} else
	    break;
	if (histp == prev_match) {
	    if (histp <= 0) {
		error("No matches found");
		break;
	    }
	}
	if (histp <= 0) {
	    if (search_dir ^ reverse)
		back_hist();
	    else {
		histp = ((lasthist + 1) % endhist);
		strlcpy(line, history[histp].histline, sizeof line);
	    }
	}
	found_it = 0;
#if defined REGCOMP
	if (regexec(last_search, line, 0, NULL, 0) == 0)
	    found_it++;
#elif defined RE_COMP
	if (re_exec(line) != 0)
	    found_it++;
#elif defined REGCMP
	if (regex(last_search, line) != NULL)
	    found_it++;
#else
	look_here = line;
	do_next = 0;
	while ((look_here = (char *)strchr(look_here, *last_search)) != NULL &&
		!found_it && !do_next) {

	    if (strncmp(look_here, last_search, strlen(last_search)) == 0)
		found_it++;
	    else if (look_here < line + strlen(line) - 1)
	        look_here++;
	    else
		do_next++;
	}
#endif
	if (histp == prev_match)
	    break;
    } while (!found_it);
    if (found_it) {
	error("History line %d", endhist - lasthist + histp);
    } else {
	error("No matches found");
    }
    edit_mode();
    linelim = strlen(line) - 1;
}

#if !defined(MSDOS)
void
write_hist(void)
{
	int i;
	FILE *fp, *tmpfp = NULL;

	if (!*histfile)
		return;
	if (histsessionnew < HISTLEN) {
		/* write the new history for this session to a tmp file */
		tmpfp = tmpfile();
		for (i = 1; i <= histsessionnew; i++) {
			histsessionstart = histsessionstart % endhist + 1;
			if (history[histsessionstart].len > 40)
			fprintf(tmpfp, "%s\n", history[histsessionstart].histline);
		}
		fseek(tmpfp, 0, SEEK_SET);

		/* re-read the main history, then read back in the saved session hist*/
		histp = 0;
		lasthist = 0;
		endhist = -1;
		read_hist();
		readhistfile(tmpfp);

		if (fclose(tmpfp) == EOF) {
			error("fclose(tmpfile()): %s", strerror(errno));
		}
	}

	/* now write to whole lot out to the proper save file */
	if (findhome(histfile, sizeof histfile) && (fp = fopen(histfile, "w"))
			!= NULL) {
		for (i = 1; i <= endhist; i++) {
			lasthist = lasthist % endhist + 1;
			if (history[lasthist].len > 40)
				fprintf(fp, "%s\n", history[lasthist].histline);
		}

		if (fclose(fp) == EOF) {
			error("fclose(%s): %s", histfile, strerror(errno));
		}
	}
}

static void
readhistfile(FILE *fp)
{
	if (!*histfile)
		return;
	while (fgets(line, FBUFLEN, fp)) {
		line[strlen(line)-1] = '\0'; /* chop the \n */
		save_hist();
	}
}

void
read_hist(void)
{
	FILE *fp;

	if (!*histfile)
		return;

	if (findhome(histfile, sizeof histfile) && (fp = fopen(histfile, "r"))
			!= NULL)
	{
		readhistfile(fp);

		if (fclose(fp) == EOF) {
			error("fclose(%s): %s", histfile, strerror(errno));
		}
	}

	histsessionstart = lasthist;
	histsessionnew = 0;
}
#endif /* !defined(MSDOS) */

static void
col_0(void) {
    linelim = 0;
}

static void
last_col(void) {
    linelim = strlen(line);
    if (linelim > 0 && mode_ind == 'e')
	--linelim;
}

static int
find_char(int a, int n) {
	register int i;

	if (findchar)
		finddir = n;
	findchar = vigetch();
	if (doti > 0)
		switch (dotb[doti - 1]) {
		case 'f': case 'F': case 't': case 'T':
			savedot(findchar);
		default:
			break;
		}
	i = linelim;
	while (a--) {
		i += n;
		while (i >= 0 && line[i] && line[i] != findchar)
			i += n;
		if (i < 0 || !line[i]) {
			i = linelim;
			break;
		}
	}
	findfunc = 'f';
	return i;
}

static int
to_char(int a, int n)
{
    register int i;
    int tmp = linelim;

    if (linelim + n >= 0 && (size_t)(linelim + n) < strlen(line))
	linelim += n;
    i = find_char(a, n);
    if (i != linelim)
	i -= n;
    linelim = tmp;
    findfunc = 't';

    return (i);
}

static void
match_paren(void) {
    int nest = 1;
    int tmp = linelim;

    if (line[linelim] == '(') {
	while (nest && ++linelim >= 0 && line[linelim]) {
	    if (line[linelim] == '(')
		nest++;
	    else if (line[linelim] == ')')
		nest--;
	}
	if (line[linelim] != ')')
	    linelim = tmp;
    }
    else if (line[linelim] == ')') {
	while (nest && --linelim >= 0 && line[linelim]) {
	    if (line[linelim] == ')')
		nest++;
	    else if (line[linelim] == '(')
		nest--;
	}
	if (line[linelim] != '(')
	    linelim = tmp;
    }
}

/* If save is 0, remember the current position.  Otherwise, if the current
 * cell has changed since the last remember(0), save the remembered location
 * for the `, ', and c comands.
 */
void
remember(int save)
{
    static int remrow, remcol, remstrow, remstcol;

    if (save && (currow != remrow || curcol != remcol ||
	    strow != remstrow || stcol != remstcol)) {
	savedrow[0] = remrow;
	savedcol[0] = remcol;
	savedstrow[0] = remstrow;
	savedstcol[0] = remstcol;
    } else {
	remrow = currow;
	remcol = curcol;
	remstrow = strow;
	remstcol = stcol;
    }
}

void
gohome(void) {
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
	if (currow >= fr->ir_left->row &&
		currow <= fr->ir_right->row &&
		curcol >= fr->ir_left->col &&
		curcol <= fr->ir_right->col &&
		(currow > fr->ir_left->row ||
		curcol > fr->ir_left->col)) {
	    currow = fr->ir_left->row;
	    curcol = fr->ir_left->col;
	} else if (currow > fr->or_left->row ||
		curcol > fr->or_left->col) {
	    currow = fr->or_left->row;
	    curcol = fr->or_left->col;
	} else {
	    currow = 0;
	    curcol = 0;
	}
    } else {
	currow = 0;
	curcol = 0;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
    FullUpdate++;
}

void
leftlimit(void) {
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
	if (currow >= fr->ir_left->row &&
		currow <= fr->ir_right->row &&
		curcol > fr->ir_left->col &&
		curcol <= fr->ir_right->col)
	    curcol = fr->ir_left->col;
	else if (curcol > fr->or_left->col &&
		curcol <= fr->or_right->col)
	    curcol = fr->or_left->col;
	else
	    curcol = 0;
    } else
	curcol = 0;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

void
rightlimit(void) {
    register struct ent *p;
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
	if (currow >= fr->ir_left->row &&
		currow <= fr->ir_right->row &&
		curcol >= fr->ir_left->col &&
		curcol < fr->ir_right->col)
	    curcol = fr->ir_right->col;
	else if (curcol >= fr->or_left->col &&
		curcol < fr->or_right->col)
	    curcol = fr->or_right->col;
	else {
	    curcol = maxcols - 1;
	    while (!VALID_CELL(p, currow, curcol) &&
		    curcol > fr->or_right->col)
		curcol--;
	    if ((fr = find_frange(currow, curcol)))
		curcol = fr->or_right->col;
	}
    } else {
	curcol = maxcols - 1;
	while (!VALID_CELL(p, currow, curcol) && curcol > 0)
	    curcol--;
	if ((fr = find_frange(currow, curcol)))
	    curcol = fr->or_right->col;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

void
gototop(void) {
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
	if (curcol >= fr->ir_left->col &&
		curcol <= fr->ir_right->col &&
		currow > fr->ir_left->row &&
		currow <= fr->ir_right->row)
	    currow = fr->ir_left->row;
	else if (currow > fr->or_left->row &&
		currow <= fr->or_right->row)
	    currow = fr->or_left->row;
	else
	    currow = 0;
    } else
	currow = 0;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

void
gotobottom(void) {
    register struct ent *p;
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
	if (curcol >= fr->ir_left->col &&
		curcol <= fr->ir_right->col &&
		currow >= fr->ir_left->row &&
		currow < fr->ir_right->row)
	    currow = fr->ir_right->row;
	else if (currow >= fr->or_left->row &&
		currow < fr->or_right->row)
	    currow = fr->or_right->row;
	else {
	    currow = maxrows - 1;
	    while (!VALID_CELL(p, currow, curcol) &&
		    currow > fr->or_right->row)
		currow--;
	    if ((fr = find_frange(currow, curcol)))
		currow = fr->or_right->row;
	}
    } else {
	currow = maxrows - 1;
	while (!VALID_CELL(p, currow, curcol) && currow > 0)
	    currow--;
	if ((fr = find_frange(currow, curcol)))
	    currow = fr->or_right->row;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

static void
dogoto(void) {
    static char		*tempc = NULL;
    static unsigned	templen = 0;
    int			tempi;

    if (strlen(line) + 1 > templen) {
    	templen = strlen(line) + 40;
	tempc = scxrealloc(tempc, templen);
    }

    strlcpy(tempc, line, templen);
    tempi = linelim;

    /* Can't switch back to navigate mode if insert_mode() is used here
     * instead of toggle_navigate_mode(), which is what we want when doing
     * a goto from within navigate mode.
     */
    insert_mode();
    /* Tempted as I was, I had to resist making this "Where would you like
     * to go today?" - CRM :)
     */
    query("goto where?", NULL);
    if (linelim >= 0) {
	memmove(line + 5, line, strlen(line) + 1);
	strlcpy(line, "goto ", 5);
	linelim = 0;
	yyparse();
    }

    strlcpy(line, tempc, sizeof line);
    linelim = tempi;
    /* Now we need to change back to navigate mode ourselves so that
     * toggle_navigate_mode() will work properly again.
     */
    mode_ind = 'v';
    mode = NAVIGATE_MODE;
    if (!showrange)
	toggle_navigate_mode();
}

void
query(const char *s, char *data)
{
    int c;

    insert_mode();
    if (data != NULL) {
	strlcpy(line, data, sizeof line);
	linelim = strlen(line);
    } else {
    	*line = '\0';
	linelim = 0;
    }
    if (s != NULL) {
	error("%s", s);
    }

    while (linelim >= 0) {
	update(0);
	switch (c = nmgetch()) {
	    case ctl('m'):
		CLEAR_LINE;
		return;
	    case ctl('g'):
		line[0] = '\0';
		linelim = -1;
		CLEAR_LINE;
		update(0);
		return;
	    case ctl('l'):
		FullUpdate++;
		clearok(stdscr,1);
		update(1);
		break;
	    default:
		write_line(c);
	}
    }
}

#ifdef NCURSES_MOUSE_VERSION
void
mouse_set_pos(int m) {
	static int x0;
	switch (m) {
	case 1:
		x0 = mevent.x;
		break;
	case 2:
		if (x0 != mevent.x)
			break;
	default:
		linelim = mevent.x - 3;
	}
}
#endif
