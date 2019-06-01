/*	SC	A Spreadsheet Calculator
 *		Command routines
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *
 *		$Revision: 7.16 $
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <time.h>
#include <utime.h>
#include <sys/file.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#ifndef MSDOS
#include <unistd.h>
#endif
#include <limits.h>
#include "compat.h"
#include "sc.h"

void	syncref(register struct enode *e);
void	unspecial(FILE *f, char *str, int delim);
static struct ent *deldata1(void);
static void deldata2(struct ent *obuf);

/* a linked list of free [struct ent]'s, uses .next as the pointer */
extern	struct ent *freeents;

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
extern	struct enode *freeenodes;

#define DEFCOLDELIM ':'

extern	char *scext;
extern	char *ascext;
extern	char *tbl0ext;
extern	char *tblext;
extern	char *latexext;
extern	char *slatexext;
extern	char *texext;
extern	int  Vopt;
extern	struct go_save gs;
int macrofd;
int cslop;
struct impexfilt *filt = NULL; /* root of list of impex filters */

/* copy the current row (currow) and place the cursor in the new row */
void
duprow(void) {
    int c1, c2, coltmp = curcol;
    struct frange *fr;

    fr = find_frange(currow, curcol);
    c1 = fr ? fr->or_left->col : 0;
    c2 = fr ? fr->or_right->col : maxcol;

    if (currow >= maxrows - 1 || (!fr && maxrow >= maxrows - 1) ||
	    (fr && fr->or_right->row >= maxrows - 1)) {
	if (!growtbl(GROWROW, 0, 0))
		return;
    }
    modflg++;
    insertrow(1, 1);
    for (curcol = c1; curcol <= c2; curcol++) {
	register struct ent *p = *ATBL(tbl, currow - 1, curcol);
	if (p) {
	    register struct ent *n;
	    n = lookat(currow, curcol);
	    (void) copyent(n, p, 1, 0, 0, 0, maxrow, maxcol, 0);
	}
    }
    curcol = coltmp;
}

/* copy the current column (curcol) and place the cursor in the new column */
void
dupcol(void) {
    int rowtmp = currow;

    if (curcol >= maxcols - 1 || maxcol >= maxcols - 1) {
	if (!growtbl(GROWCOL, 0, 0))
		return;
    }
    modflg++;
    insertcol(1, 1);
    for (currow = 0; currow <= maxrow; currow++) {
	register struct ent *p = *ATBL(tbl, currow, curcol - 1);
	if (p) {
	    register struct ent *n;
	    n = lookat(currow, curcol);
	    copyent(n, p, 0, 1, 0, 0, maxrow, maxcol, 0);
	}
    }
    currow = rowtmp;
}

/* Insert 'arg' rows.  The row(s) will be inserted before currow if delta
 * is 0; after if it is 1.
 */
void
insertrow(int arg, int delta)
{
    int	r, c, i;
    struct ent	**tmprow, **pp;
    int lim = maxrow - currow + 1;
    struct frange *fr;

    if (currow > maxrow)
	maxrow = currow;
    maxrow += arg;
    lim = maxrow - lim + delta;

    if ((maxrow >= maxrows) && !growtbl(GROWROW, maxrow, 0))
	return;

    if ((fr = find_frange(currow + delta, curcol))) {
	move_area(currow + arg + delta, fr->or_left->col, currow + delta,
		fr->or_left->col, fr->or_right->row, fr->or_right->col);
	if (!delta && fr->ir_left->row == currow + arg)
	    fr->ir_left = lookat(fr->ir_left->row - arg, fr->ir_left->col);
	if (delta && fr->ir_right->row == currow)
	    fr->ir_right = lookat(fr->ir_right->row + arg, fr->ir_right->col);

	for (i = 0; i < 37; i++) {	/* update all marked cells */
	    if (savedrow[i] >= currow + delta &&
		    savedcol[i] >= fr->or_left->col &&
		    savedcol[i] <= fr->or_right->col)
		savedrow[i] += arg;
	    if (savedstrow[i] >= currow + delta &&
		    savedstcol[i] >= fr->or_left->col &&
		    savedstcol[i] <= fr->or_right->col)
		savedstrow[i] += arg;
	}
	if (gs.g_row >= currow + delta &&
		gs.g_col >= fr->or_left->col &&
		gs.g_col <= fr->or_right->col)
	    gs.g_row += arg;
	if (gs.g_lastrow >= currow + delta &&
		gs.g_lastcol >= fr->or_left->col &&
		gs.g_lastcol <= fr->or_right->col)
	    gs.g_lastrow += arg;
	if (gs.strow >= currow + delta &&
		gs.stcol >= fr->or_left->col &&
		gs.stcol <= fr->or_right->col)
	    gs.strow += arg;
	for (r = 0; r <= maxrow; r++) {
	    for (c = 0; c <= maxcol; c++) {
		pp = ATBL(tbl, r, c);
		if (*pp) {
		    if ((*pp)->nrow >= currow + delta &&
			    (*pp)->ncol >= fr->or_left->col &&
			    (*pp)->ncol <= fr->or_right->col)
			((*pp)->nrow) += arg;
		    if ((*pp)->nlastrow >= currow + delta &&
			    (*pp)->nlastcol >= fr->or_left->col &&
			    (*pp)->nlastcol <= fr->or_right->col)
			((*pp)->nlastrow) += arg;
		}
	    }
	}
    } else {
	
	/*
	 * save the last active row+1, shift the rows downward, put the last
	 * row in place of the first
	 */
	tmprow = tbl[maxrow];
	for (r = maxrow; r > lim; r--) {
	    row_hidden[r] = row_hidden[r-arg];
	    row_hidden[r-arg] = row_hidden[r-1];
	    tbl[r] = tbl[r-arg];
	    tbl[r-arg] = tbl[r-1];
	    pp = ATBL(tbl, r, 0);
	    for (c = 0; c < maxcols; c++, pp++)
		if (*pp)
		    (*pp)->row = r;
	}
	tbl[r] = tmprow;		/* the last row was never used.... */

	for (i = 0; i < 37; i++) {	/* update all marked cells */
	    if (savedrow[i] >= currow + delta)
		savedrow[i] += arg;
	    if (savedstrow[i] >= currow + delta)
		savedstrow[i] += arg;
	}
	if (gs.g_row >= currow + delta)
	    gs.g_row += arg;
	if (gs.g_lastrow >= currow + delta)
	    gs.g_lastrow += arg;
	if (gs.strow >= currow + delta)
	    gs.strow += arg;
	for (r = 0; r <= maxrow; r++) {
	    for (c = 0; c <= maxcol; c++) {
		pp = ATBL(tbl, r, c);
		if (*pp) {
		    if ((*pp)->nrow >= currow + delta)
			((*pp)->nrow) += arg;
		    if ((*pp)->nlastrow >= currow + delta)
			((*pp)->nlastrow) += arg;
		}
	    }
	}
    }
    fix_ranges(currow + arg * (1 - delta), -1, currow + arg * (1 - delta), -1,
	    delta?0:arg, delta?arg:0);
    currow += delta;
    FullUpdate++;
    modflg++;
}

/* Insert 'arg' cols.  The col(s) will be inserted before curcol if delta
 * is 0; after if it is 1.
 */
void
insertcol(int arg, int delta)
{
    int r, c;
    register struct ent **pp;
    int lim = maxcol - curcol - delta + 1;
    struct frange *fr;

    if (curcol + delta > maxcol)
	maxcol = curcol + delta;
    maxcol += arg;

    if ((maxcol >= maxcols) && !growtbl(GROWCOL, 0, maxcol))
	return;

    for (c = maxcol; c >= curcol + delta + arg; c--) {
	fwidth[c] = fwidth[c-arg];
	precision[c] = precision[c-arg];
	realfmt[c] = realfmt[c-arg];
	col_hidden[c] = col_hidden[c-arg];
    }
    for (c = curcol + delta; c - curcol - delta < arg; c++) {
    	fwidth[c] = DEFWIDTH;
	precision[c] =  DEFPREC;
	realfmt[c] = DEFREFMT;
	col_hidden[c] = FALSE;
    }
	
    for (r=0; r <= maxrow; r++) {
	pp = ATBL(tbl, r, maxcol);
	for (c = lim; --c >= 0; pp--)
	    if ((pp[0] = pp[-arg]))
		pp[0]->col += arg;

	pp = ATBL(tbl, r, curcol + delta);
	for (c = curcol + delta; c - curcol - delta < arg; c++, pp++)
	    *pp = (struct ent *)0;
    }

    /* Update all marked cells. */
    for (c=0; c < 37; c++) {
	if (savedcol[c] >= curcol + delta)
	    savedcol[c] += arg;
	if (savedstcol[c] >= curcol + delta)
	    savedstcol[c] += arg;
    }
    if (gs.g_col >= curcol + delta)
	gs.g_col += arg;
    if (gs.g_lastcol >= curcol + delta)
	gs.g_lastcol += arg;
    if (gs.stcol >= curcol + delta)
	gs.stcol += arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
	for (c = 0; c <= maxcol; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp) {
		if ((*pp)->ncol >= curcol + delta)
		    ((*pp)->ncol) += arg;
		if ((*pp)->nlastcol >= curcol + delta)
		    ((*pp)->nlastcol) += arg;
	    }
	}
    }
    if (!delta && (fr = find_frange(currow, curcol)) &&
	    fr->ir_left->col == curcol + arg)
	fr->ir_left = lookat(fr->ir_left->row, fr->ir_left->col - arg);
    if (delta && (fr = find_frange(currow, curcol)) &&
	    fr->ir_right->col == curcol)
	fr->ir_right = lookat(fr->ir_right->row, fr->ir_right->col + arg);
    fix_ranges(-1, curcol + arg * (1 - delta), -1, curcol + arg * (1 - delta),
	    delta?0:arg, delta?arg:0);
    curcol += delta;
    FullUpdate++;
    modflg++;
}

/* delete 'arg' rows starting at currow (deletes from currow downward) */
void
deleterow(register int arg)
{
    int i;
    int rs = maxrow - currow + 1;
    struct frange *fr;
    struct ent *obuf = NULL;
    char buf[50];

    if ((fr = find_frange(currow, curcol)))
	rs = fr->or_right->row - currow + 1;
    if (rs - arg < 0) {
	rs = rs > 0 ? rs : 0;
	snprintf(buf, sizeof buf, "Can't delete %d row%s %d row%s left", arg,
		(arg != 1 ? "s," : ","), rs, (rs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    if (fr) {
	if (any_locked_cells(currow, fr->or_left->col,
		currow + arg - 1, fr->or_right->col)) {
	    error("Locked cells encountered. Nothing changed");
	} else {
	    FullUpdate++;
	    modflg++;
	    obuf = deldata1();
	    erase_area(currow, fr->or_left->col, currow + arg - 1,
		    fr->or_right->col, 0);
	    fix_ranges(currow, -1, currow + arg - 1, -1, -1, -1);
	    deldata2(obuf);
	    if (currow + arg > fr->ir_right->row &&
		    fr->ir_right->row >= currow)
		fr->ir_right = lookat(currow - 1, fr->ir_right->col);
	    if (currow + arg > fr->or_right->row)
		fr->or_right = lookat(currow - 1, fr->or_right->col);
	    else
		move_area(currow, fr->or_left->col, currow + arg,
			fr->or_left->col, fr->or_right->row, fr->or_right->col);
	    if (fr->ir_left->row > fr->ir_right->row)
		add_frange(fr->or_left, fr->or_right, NULL, NULL, 0, 0, 0, 0);

	    /* Update all marked cells. */
	    for (i = 0; i < 37; i++) {
		if (savedcol[i] >= fr->or_left->col &&
			savedcol[i] <= fr->or_right->col) {
		    if (savedrow[i] >= currow && savedrow[i] < currow + arg)
			savedrow[i] = savedcol[i] = -1;
		    if (savedrow[i] >= currow + arg)
			savedrow[i] -= arg;
		}
		if (savedstcol[i] >= fr->or_left->col &&
			savedstcol[i] <= fr->or_right->col) {
		    if (savedstrow[i] >= currow && savedstrow[i] < currow + arg)
			savedstrow[i] = currow;
		    if (savedstrow[i] >= currow + arg)
			savedstrow[i] -= arg;
		}
	    }
	    if (gs.g_col >= fr->or_left->col &&
		    gs.g_col <= fr->or_right->col) {
		if (gs.g_row >= currow && gs.g_row < currow + arg)
		    gs.g_row = currow;
		if (gs.g_row >= currow + arg)
		    gs.g_row -= arg;
	    }
	    if (gs.g_lastcol >= fr->or_left->col &&
		    gs.g_lastcol <= fr->or_right->col) {
		if (gs.g_lastrow >= currow && gs.g_lastrow < currow + arg)
		    gs.g_lastrow = currow - 1;
		if (gs.g_lastrow >= currow + arg)
		    gs.g_lastrow -= arg;
	    }
	    if (gs.g_row > gs.g_lastrow)
		gs.g_row = gs.g_col = -1;
	    if (gs.stcol >= fr->or_left->col &&
		    gs.stcol <= fr->or_right->col) {
		if (gs.strow >= currow && gs.strow < currow + arg)
		    gs.strow = currow;
		if (gs.strow >= currow + arg)
		    gs.strow -= arg;
	    }
	}
    } else {
	
	if (any_locked_cells(currow, 0, currow + arg - 1, maxcol)) {
	    error("Locked cells encountered. Nothing changed");
	} else {
	    obuf = deldata1();
	    erase_area(currow, 0, currow + arg - 1, maxcol, 0);
	    fix_ranges(currow, -1, currow + arg - 1, -1, -1, -1);
	    closerow(currow, arg);
	    deldata2(obuf);
	}
    }
}

static struct ent *
deldata1(void) {
    int i;
    struct ent *obuf = NULL;
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 1];
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 1];
    delbuf[DELBUFSIZE - 1] = NULL;
    delbuffmt[DELBUFSIZE - 1] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
	if (delbuf[i] == delbuf[dbidx]) {
	    delbuf[dbidx] = NULL;
	    delbuffmt[dbidx] = NULL;
	    break;
	}
    }
    flush_saved();
    if (qbuf) {
	if (dbidx < 0) dbidx++;
	delbuf[dbidx] = delbuf[qbuf];
	delbuffmt[dbidx] = delbuffmt[qbuf];
	flush_saved();
	obuf = delbuf[qbuf];	/* orig. contents of the del. buffer */
    }
    sync_refs();
    return obuf;
}

static void
deldata2(struct ent *obuf) {
    int i;
    struct ent *p;
    for (i = 0; i < DELBUFSIZE; i++)
	if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
	    delbuf[i] = delbuf[dbidx];
	    delbuffmt[i] = delbuffmt[dbidx];
	}
    qbuf = 0;
    for (i = DELBUFSIZE - 1; i > DELBUFSIZE - 9; i--) {
	delbuf[i] = delbuf[i-1];
	delbuffmt[i] = delbuffmt[i-1];
    }
    delbuf[DELBUFSIZE - 9] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 9] = delbuffmt[dbidx];
    for (p = delbuf[dbidx]; p; p = p->next)
	p->flags &= ~MAY_SYNC;
}

void
yankrow(int arg) {
    int rs = maxrow - currow + 1;
    int i, qtmp;
    char buf[50];
    struct frange *fr;
    struct ent *obuf = NULL;

    if ((fr = find_frange(currow, curcol)))
	rs = fr->or_right->row - currow + 1;
    if (rs - arg < 0) {
	rs = rs > 0 ? rs : 0;
	snprintf(buf, sizeof buf, "Can't yank %d row%s %d row%s left", arg,
		(arg != 1 ? "s," : ","), rs, (rs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    sync_refs();
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[DELBUFSIZE - 10] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
	if (delbuf[i] == delbuf[dbidx]) {
	    delbuf[dbidx] = NULL;
	    delbuffmt[dbidx] = NULL;
	    break;
	}
    }
    flush_saved();
    if (qbuf) {
	if (dbidx < 0) dbidx++;
	delbuf[dbidx] = delbuf[qbuf];
	delbuffmt[dbidx] = delbuffmt[qbuf];
	flush_saved();
	obuf = delbuf[qbuf];	/* orig. contents of the del. buffer */
    }
    qtmp = qbuf;
    qbuf = 0;
    if (fr) {
	yank_area(currow, fr->or_left->col, currow + arg - 1,
		fr->or_right->col);
    } else {
	yank_area(currow, 0, currow + arg - 1, maxcol);
    }
    qbuf = qtmp;
    for (i = 0; i < DELBUFSIZE; i++)
	if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
	    delbuf[i] = delbuf[dbidx];
	    delbuffmt[i] = delbuffmt[dbidx];
	}
    qbuf = 0;
    delbuf[DELBUFSIZE - 10] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 10] = delbuffmt[dbidx];
}

void
yankcol(int arg) {
    int cs = maxcol - curcol + 1;
    int i, qtmp;
    char buf[50];
    struct ent *obuf = NULL;

    if (cs - arg < 0) {
    	cs = cs > 0 ? cs : 0;
	snprintf(buf, sizeof buf, "Can't yank %d column%s %d column%s left",
	    arg, (arg != 1 ? "s," : ","), cs, (cs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    sync_refs();
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[DELBUFSIZE - 10] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
	if (delbuf[i] == delbuf[dbidx]) {
	    delbuf[dbidx] = NULL;
	    delbuffmt[dbidx] = NULL;
	    break;
	}
    }
    flush_saved();
    if (qbuf) {
	if (dbidx < 0) dbidx++;
	delbuf[dbidx] = delbuf[qbuf];
	delbuffmt[dbidx] = delbuffmt[qbuf];
	flush_saved();
	obuf = delbuf[qbuf];	/* orig. contents of the del. buffer */
    }
    qtmp = qbuf;
    qbuf = 0;
    yank_area(0, curcol, maxrow, curcol + arg - 1);
    qbuf = qtmp;
    for (i = 0; i < DELBUFSIZE; i++)
	if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
	    delbuf[i] = delbuf[dbidx];
	    delbuffmt[i] = delbuffmt[dbidx];
	}
    qbuf = 0;
    delbuf[DELBUFSIZE - 10] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 10] = delbuffmt[dbidx];
}

/* ignorelock is used when sorting so that locked cells can still be sorted */

void
erase_area(int sr, int sc, int er, int ec, int ignorelock)
{
    int r, c;
    struct ent **pp;

    if (sr > er) {
	r = sr; sr = er; er = r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec = c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    /* Do a lookat() for the upper left and lower right cells of the range
     * being erased to make sure they are included in the delete buffer so
     * that pulling cells always works correctly even if the cells at one
     * or more edges of the range are all empty.
     */
    (void) lookat(sr, sc);
    (void) lookat(er, ec);

    delbuffmt[++dbidx] = scxmalloc((4*(ec-sc+1)+(er-sr+1))*sizeof(char));
    for (c = sc; c <= ec; c++) {
	delbuffmt[dbidx][4*(c-sc)] = (char)fwidth[c];
	delbuffmt[dbidx][4*(c-sc)+1] = (char)precision[c];
	delbuffmt[dbidx][4*(c-sc)+2] = (char)realfmt[c];
	delbuffmt[dbidx][4*(c-sc)+3] = (char)col_hidden[c];
    }
    for (r = sr; r <= er; r++) {
	for (c = sc; c <= ec; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp && (!((*pp)->flags&IS_LOCKED) || ignorelock)) {
		free_ent(*pp, 0);
		*pp = NULL;
	    }
	}
	delbuffmt[dbidx][4*(ec-sc+1)+(r-sr)] = (char)row_hidden[r];
    }
}

void
yank_area(int sr, int sc, int er, int ec)
{
    int r, c;

    if (sr > er) {
	r = sr; sr = er; er = r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec = c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    r = currow;
    currow = sr;
    c = curcol;
    curcol = sc;

    erase_area(sr, sc, er, ec, 0);
    pullcells('p');

    currow = r;
    curcol = c;
}

void
move_area(int dr, int dc, int sr, int sc, int er, int ec)
{
    struct ent *p;
    struct ent **pp;
    int deltar, deltac;
    int r, c;

    if (sr > er) {
	r = sr; sr = er; er = r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec = c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    r = currow;
    currow = sr;
    c = curcol;
    curcol = sc;

    /* First we erase the source range, which puts the cells on the delete
     * buffer stack.
     */
    erase_area(sr, sc, er, ec, 0);

    currow = r;
    curcol = c;
    deltar = dr - sr;
    deltac = dc - sc;

    /* Now we erase the destination range, which adds it to the delete buffer
     * stack, but then we flush it off.  We then move the original source
     * range from the stack to the destination range, adjusting the addresses
     * as we go, leaving the stack in its original state.
     */
    erase_area(dr, dc, er + deltar, ec + deltac, 0);
    flush_saved();
    for (p = delbuf[dbidx]; p; p = p->next) {
	pp = ATBL(tbl, p->row + deltar, p->col + deltac);
	*pp = p;
	p->row += deltar;
	p->col += deltac;
	p->flags &= ~IS_DELETED;
    }
    delbuf[dbidx] = NULL;
    delbuffmt[dbidx--] = NULL;
}

/*
 * deletes the expression associated w/ a cell and turns it into a constant
 * containing whatever was on the screen
 */
void
valueize_area(int sr, int sc, int er, int ec)
{
    int r, c;
    struct ent *p;

    if (sr > er) {
	r = sr; sr = er; er= r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec= c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    for (r = sr; r <= er; r++) {
	for (c = sc; c <= ec; c++) {
	    p = *ATBL(tbl, r, c);
	    if (p && p->flags&IS_LOCKED) {
		error(" Cell %s%d is locked", coltoa(c), r);
		continue;
	    }
	    if (p && p->expr) {
		efree(p->expr);
		p->expr = (struct enode *)0;
		p->flags &= ~IS_STREXPR;
	    }
	}
    }
}

void
pullcells(int to_insert)
{
    struct ent *obuf;
    struct ent *p, *n;
    struct ent **pp;
    int deltar, deltac;
    int minrow, mincol;
    int mxrow, mxcol;
    int numrows, numcols;
    int i;
    struct frange *fr;
    
    if (qbuf && delbuf[qbuf]) {
	delbuf[++dbidx] = delbuf[qbuf];
	delbuffmt[dbidx] = delbuffmt[qbuf];
    }
    obuf = delbuf[dbidx];	/* orig. contents of the del. buffer */

    if ((qbuf && !delbuf[qbuf]) || dbidx < 0) {
	error("No data to pull");
	qbuf = 0;
	return;
    }

    minrow = maxrows; 
    mincol = maxcols;
    mxrow = 0;
    mxcol = 0;

    for (p = delbuf[dbidx]; p; p = p->next) {
	if (p->row < minrow)
	    minrow = p->row;
	if (p->row > mxrow)
	    mxrow = p->row;
	if (p->col < mincol)
	    mincol = p->col;
	if (p->col > mxcol)
	    mxcol = p->col;
	p->flags |= MAY_SYNC;
    }

    numrows = mxrow - minrow + 1;
    numcols = mxcol - mincol + 1;
    deltar = currow - minrow;
    deltac = curcol - mincol;

    if (to_insert == 'C') {
	minrow = 0;
	mincol = 0;
	mxrow = maxrows;
	mxcol = maxcols;
    }

    if (to_insert == 'r') {
	insertrow(numrows, 0);
	if ((fr = find_frange(currow, curcol)))
	    deltac = fr->or_left->col - mincol;
	else {
	    for (i = 0; i < numrows; i++)
		row_hidden[currow+i] = delbuffmt[dbidx][4*numcols+i];
	    deltac = 0;
	}
    } else if (to_insert == 'c') {
	insertcol(numcols, 0);
	for (i = 0; i < numcols; i++) {
	    fwidth[curcol+i] = delbuffmt[dbidx][4*i];
	    precision[curcol+i] = delbuffmt[dbidx][4*i+1];
	    realfmt[curcol+i] = delbuffmt[dbidx][4*i+2];
	    col_hidden[curcol+i] = delbuffmt[dbidx][4*i+3];
	}
	deltar = 0;
    } else if (to_insert == 'x') {	/* Do an exchange. */
	struct ent *tmpbuf;
	char *tmpfmt;

	/* Save the original contents of the destination range on the
	 * delete buffer stack in preparation for the exchange, then swap
	 * the top two pointers on the stack, so that the original cells
	 * to be pulled are still on top.
	 */
	erase_area(minrow + deltar, mincol + deltac, mxrow + deltar,
		mxcol + deltac, 0);
	tmpbuf = delbuf[dbidx];
	delbuf[dbidx] = delbuf[dbidx - 1];
	delbuf[dbidx - 1] = tmpbuf;
	tmpfmt = delbuffmt[dbidx];
	delbuffmt[dbidx] = delbuffmt[dbidx - 1];
	delbuffmt[dbidx - 1] = tmpfmt;
    } else if (to_insert == 'p') {
	erase_area(minrow + deltar, mincol + deltac, mxrow + deltar,
		mxcol + deltac, 0);
	sync_refs();
	flush_saved();
    } else if (to_insert == 't') {
	erase_area(minrow + deltar, mincol + deltac,
		minrow + deltar + mxcol - mincol,
		mincol + deltac + mxrow - minrow, 0);
	sync_refs();
	flush_saved();
    }

    FullUpdate++;
    modflg++;

    /* At this point, we copy the cells from the delete buffer into the
     * destination range.
     */
    for (p = delbuf[dbidx]; p; p = p->next) {
	if (to_insert == 't')	/* Transpose rows and columns while pulling. */
	    n = lookat(minrow + deltar + p->col - mincol,
		    mincol + deltac + p->row - minrow);
	else
	    n = lookat(p->row + deltar, p->col + deltac);
	copyent(n, p, deltar, deltac, minrow, mincol, mxrow, mxcol, to_insert);
    }

    /* Now exchange them so that the original cells from the delete buffer
     * are in the destination range instead of the copies.  When doing a
     * "pull exchange" ("px" or "pullxchg"), exchange the original contents
     * of the destination range with the contents of the delete buffer
     * instead.  Don't do this if transposing or merging (including merging
     * cell formats), or if the expressions in the destination cells have
     * been adjusted during a copy.
     */
    if (to_insert != 't' && to_insert != 'm' && to_insert != 'f' &&
	    to_insert != 'C') {
	if (to_insert == 'x') {
	    struct ent *tmpbuf = delbuf[dbidx];
	    char *tmpfmt = delbuffmt[dbidx];

	    delbuf[dbidx] = delbuf[dbidx - 1];
	    delbuf[dbidx - 1] = tmpbuf;
	    delbuffmt[dbidx] = delbuffmt[dbidx - 1];
	    delbuffmt[dbidx - 1] = tmpfmt;
	} else
	    for (p = delbuf[dbidx++]; p; p = p->next) {
		pp = ATBL(tbl, p->row + deltar, p->col + deltac);
		if (*pp && !((*pp)->flags & IS_LOCKED)) {
		    free_ent(*pp, 1);
		    *pp = NULL;
		}
	    }
	for (p = delbuf[dbidx - 1]; p; p = p->next) {
	    pp = ATBL(tbl, p->row + deltar, p->col + deltac);
	    *pp = p;
	    p->row += deltar;
	    p->col += deltac;
	    p->flags &= ~IS_DELETED;
	}
	delbuf[dbidx - 1] = delbuf[dbidx];
	delbuf[dbidx--] = NULL;

	sync_refs();
	/*
	 * Now change the cell addresses in the delete buffer to match
	 * where the original cells came from.
	 */
	for (p = delbuf[dbidx]; p; p = p->next) {
	    p->row -= deltar;
	    p->col -= deltac;
	}
    } else
	sync_refs();
    
    /* Now make sure all references to the pulled cells in all named buffers
     * point to the new set of cells in the delete buffer.
     */
    for (i = 0; i < DELBUFSIZE; i++)
	if (delbuf[i] == obuf) {
	    delbuf[i] = delbuf[dbidx];
	    delbuffmt[i] = delbuffmt[dbidx];
	}
    if (qbuf && delbuf[qbuf]) {
	delbuf[dbidx] = NULL;
	delbuffmt[dbidx--] = NULL;
    }
    qbuf = 0;
}

void
colshow_op(void) {
    register int i,j;
    for (i = 0; i < maxcols; i++)
	if (col_hidden[i]) 
	    break;
    for(j = i; j < maxcols; j++)
	if (!col_hidden[j])
	    break;
    j--;
    if (i >= maxcols) {
	error("No hidden columns to show");
    } else {
	snprintf(line, sizeof line, "show %s:", coltoa(i));
	linelim = strlen(line);
	snprintf(line + linelim, sizeof(line) - linelim, "%s", coltoa(j));
	linelim = strlen(line);
    }
}

void
rowshow_op(void)
{
    register int i,j;
    for (i = 0; i < maxrows; i++)
	if (row_hidden[i]) 
	    break;
    for(j = i; j < maxrows; j++)
	if (!row_hidden[j]) {
	    break;
	}
    j--;

    if (i >= maxrows) {
	error("No hidden rows to show");
    } else {
	snprintf(line, sizeof line, "show %d:%d", i, j);
        linelim = strlen(line);
    }
}

/*
 * Given a row/column command letter, emit a small menu, then read a qualifier
 * character for a row/column command and convert it to 'r' (row), 'c'
 * (column), or 0 (unknown).  If ch is 'p', three extra qualifiers, 'm', 'x',
 * and 't', are allowed.  If ch is 'Z', an extra qualifier 'Z' is allowed.
 */

int
get_rcqual(int ch)
{
    int c;

    error("%sow/column:  r: row  c: column%s",

#ifdef KEY_IC
	(ch == KEY_IC)		? "Insert r" :
#endif
	(ch == 'i')		? "Insert r" :
	(ch == 'o')		? "Open r" :
	(ch == 'a')		? "Append r" :
	(ch == 'd')		? "Delete r" :
	(ch == 'y')		? "Yank r" :
	(ch == 'p')		? "Pull r" :
	(ch == 'v')		? "Values r" :
	(ch == 'Z')		? "Zap r" :
	(ch == 's')		? "Show r" : "R",
	
	(ch == 'p')		? "  p: paste  m: merge  x: xchg  <MORE>" :
	(ch == 'Z')		? "  Z: save/exit" : "");

    (void) refresh();

    switch (c = nmgetch())
    {
	case 'r':	return ('r');

	case 'c':	return ('c');

	case 'p':	return ((ch == 'p') ? 'p' : 0);

	case 'm':	return ((ch == 'p') ? 'm' : 0);

	case 'x':	return ((ch == 'p') ? 'x' : 0);

	case 't':	return ((ch == 'p') ? 't' : 0);

	case 'f':	return ((ch == 'p') ? 'f' : 0);

	case 'C':	return ((ch == 'p') ? 'C' : 0);

	case '.':	return ((ch == 'p') ? '.' : 0);

	case 'Z':	return ((ch == 'Z') ? 'Z' : 0);

	case ESC:
	case ctl('g'):	return (ESC);

	case 'd':	if (ch == 'd') {
			    ungetch('x');
			    return (ESC);
			} else
			    return (0);

	case 'y':	if (ch == 'y') {
			    yankr(lookat(currow, curcol),
				    lookat(currow, curcol));
			    return (ESC);
			} else
			    return (0);

	case 'v':	if (ch == 'v') {
			    valueize_area(currow, curcol, currow, curcol);
			    modflg++;
			    return (ESC);
			} else
			    return (0);

	case KEY_UP:
	case KEY_DOWN:
	case KEY_PPAGE:
	case KEY_NPAGE:
	case 'j':
	case 'k':
	case 'J':
	case 'K':
	case ctl('f'):
	case ctl('b'):
	case ctl('n'):
	case ctl('p'):	if (ch == 'd')
			    snprintf(line, sizeof line, "deleterow [range] ");
			else if (ch == 'y')
			    snprintf(line, sizeof line, "yankrow [range] ");
			else if (ch == 'Z')
			    snprintf(line, sizeof line, "hide [range] ");
			else
			    return (0);
			edit_mode();
			write_line('A');
			startshow();
			showrange = SHOWROWS;
			showsr = currow;
			ungetch(c);
			return (ESC);

	case KEY_BACKSPACE:
	case KEY_LEFT:
	case KEY_RIGHT:
	case ' ':
	case 'h':
	case 'l':
	case 'H':
	case 'L':	if (ch == 'd')
			    snprintf(line, sizeof line, "deletecol [range] ");
			else if (ch == 'y')
			    snprintf(line, sizeof line, "yankcol [range] ");
			else if (ch == 'Z')
			    snprintf(line, sizeof line, "hide [range] ");
			else
			    return (0);
			edit_mode();
			write_line('A');
			startshow();
			showrange = SHOWCOLS;
			showsc = curcol;
			ungetch(c);
			return (ESC);

	default:	return (0);
    }
    /*NOTREACHED*/
}

/* delete numrow rows, starting with rs */
void
closerow(int rs, int numrow)
{
    register struct ent **pp;
    int			r, c, i;
    struct ent		**tmprow;

    if (rs + numrow - 1 > maxrow) return;
    r = rs;

/*
 * Rows are dealt with in numrow groups, each group of rows spaced numrow
 * rows apart.
 */
    for (i = 0; i < numrow; i++) {
	r = rs + i;

	/* save the first row of the group and empty it out */
	tmprow = tbl[r];
	pp = ATBL(tbl, r, 0);
	for (c = maxcol + 1; --c >= 0; pp++) {
	    if (*pp) {
		free_ent(*pp, 1);
		*pp = (struct ent *)0;
	    }
	}

	/* move the rows, put the deleted, but now empty, row at the end */
	for (; r + numrow < maxrows - 1; r += numrow) {
	    row_hidden[r] = row_hidden[r + numrow];
	    tbl[r] = tbl[r + numrow];
	    pp = ATBL(tbl, r, 0);
	    for (c = 0; c < maxcols; c++, pp++)
		if (*pp)
		    (*pp)->row = r;
	}
	tbl[r] = tmprow;
    }

    /* Update all marked cells. */
    for (i = 0; i < 37; i++) {
	if (savedrow[i] >= rs && savedrow[i] < rs + numrow)
	    savedrow[i] = savedcol[i] = -1;
	if (savedrow[i] >= rs + numrow)
	    savedrow[i] -= numrow;
	if (savedstrow[i] >= rs && savedstrow[i] < rs + numrow)
	    savedstrow[i] = rs;
	if (savedstrow[i] >= rs + numrow)
	    savedstrow[i] -= numrow;
    }
    if (gs.g_row >= rs && gs.g_row < rs + numrow)
	gs.g_row = rs;
    if (gs.g_row >= rs + numrow)
	gs.g_row -= numrow;
    if (gs.g_lastrow >= rs && gs.g_lastrow < rs + numrow)
	gs.g_lastrow = rs - 1;
    if (gs.g_lastrow >= rs + numrow)
	gs.g_lastrow -= numrow;
    if (gs.g_row > gs.g_lastrow)
	gs.g_row = gs.g_col = -1;
    if (gs.strow >= rs && gs.strow < rs + numrow)
	gs.strow = rs;
    if (gs.strow >= rs + numrow)
	gs.strow -= numrow;

    maxrow -= numrow;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
	for (c = 0; c <= maxcol; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp) {
		if ((*pp)->nrow >= rs && (*pp)->nrow < rs + numrow)
		    (*pp)->nrow = rs;
		if ((*pp)->nrow >= rs + numrow)
		    ((*pp)->nrow) -= numrow;
		if ((*pp)->nlastrow >= rs && (*pp)->nlastrow < rs + numrow)
		    (*pp)->nlastrow = rs - 1;
		if ((*pp)->nlastrow >= rs + numrow)
		    ((*pp)->nlastrow) -= numrow;
		if ((*pp)->nlastrow < (*pp)->nrow)
		    (*pp)->nrow = (*pp)->ncol = -1;
	    }
	}
    }
    FullUpdate++;
    modflg++;
}

/* delete group of columns (1 or more) */
void
closecol(int arg)
{
    int r, c, i;
    int cs = maxcol - curcol + 1;
    struct ent **pp;
    struct ent *p;
    struct ent *obuf = NULL;
    char buf[50];

    if (cs - arg < 0) {
    	cs = cs > 0 ? cs : 0;
	snprintf(buf, sizeof buf, "Can't delete %d column%s %d column%s left",
	    arg, (arg != 1 ? "s," : ","), cs, (cs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    if (any_locked_cells(0, curcol, maxrow, curcol + arg - 1)) {
	error("Locked cells encountered. Nothing changed");
	return;
    }
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 1];
    delbuf[DELBUFSIZE - 1] = NULL;
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 1];
    delbuffmt[DELBUFSIZE - 1] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
	if (delbuf[i] == delbuf[dbidx]) {
	    delbuf[dbidx] = NULL;
	    delbuffmt[dbidx] = NULL;
	    break;
	}
    }
    flush_saved();
    if (qbuf) {
	if (dbidx < 0) dbidx++;
	delbuf[dbidx] = delbuf[qbuf];
	delbuffmt[dbidx] = delbuffmt[qbuf];
	flush_saved();
	obuf = delbuf[qbuf];	/* orig. contents of the del. buffer */
    }
    sync_refs();
    erase_area(0, curcol, maxrow, curcol + arg - 1, 0);
    fix_ranges(-1, curcol, -1, curcol + arg - 1, -1, -1);
    for (i = 0; i < DELBUFSIZE; i++)
	if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
	    delbuf[i] = delbuf[dbidx];
	    delbuffmt[i] = delbuffmt[dbidx];
	}
    qbuf = 0;
    for (i = DELBUFSIZE - 1; i > DELBUFSIZE - 9; i--) {
	delbuf[i] = delbuf[i-1];
	delbuffmt[i] = delbuffmt[i-1];
    }
    delbuf[DELBUFSIZE - 9] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 9] = delbuffmt[dbidx];
    for (p = delbuf[dbidx]; p; p = p->next)
	p->flags &= ~MAY_SYNC;

    /* clear then copy the block left */
    cs = maxcols - arg - 1;
    for (r=0; r<=maxrow; r++) {
	for (c = curcol; c - curcol < arg; c++)
	    if ((p = *ATBL(tbl, r, c)))
		free_ent(p, 1);

	pp = ATBL(tbl, r, curcol);
	for (c=curcol; c <= cs; c++, pp++) {
	    if (c > cs)
		*pp = (struct ent *)0;
	    else if ((pp[0] = pp[arg]))
		pp[0]->col -= arg;
	}

	c = arg;
	for (; --c >= 0; pp++)		
	    *pp = (struct ent *)0;
    }

    for (i = curcol; i < maxcols - arg - 1; i++) {
	fwidth[i] = fwidth[i+arg];
	precision[i] = precision[i+arg];
	realfmt[i] = realfmt[i+arg];
	col_hidden[i] = col_hidden[i+arg];
    }
    for (; i < maxcols - 1; i++) {
	fwidth[i] = DEFWIDTH;
	precision[i] = DEFPREC;
	realfmt[i] = DEFREFMT;
	col_hidden[i] = FALSE;
    }

    /* Update all marked cells. */
    for (i = 0; i < 37; i++) {
	if (savedcol[i] >= curcol && savedcol[i] < curcol + arg)
	    savedrow[i] = savedcol[i] = -1;
	if (savedcol[i] >= curcol + arg)
	    savedcol[i] -= arg;
	if (savedstcol[i] >= curcol && savedstcol[i] < curcol + arg)
	    savedstcol[i] = curcol;
	if (savedstcol[i] >= curcol + arg)
	    savedstcol[i] -= arg;
    }
    if (gs.g_col >= curcol && gs.g_col < curcol + arg)
	gs.g_col = curcol;
    if (gs.g_col >= curcol + arg)
	gs.g_col -= arg;
    if (gs.g_lastcol >= curcol && gs.g_lastcol < curcol + arg)
	gs.g_lastcol = curcol - 1;
    if (gs.g_lastcol >= curcol + arg)
	gs.g_lastcol -= arg;
    if (gs.g_col > gs.g_lastcol)
	gs.g_row = gs.g_col = -1;
    if (gs.stcol >= curcol && gs.stcol < curcol + arg)
	gs.stcol = curcol;
    if (gs.stcol >= curcol + arg)
	gs.stcol -= arg;

    maxcol -= arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
	for (c = 0; c <= maxcol; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp) {
		if ((*pp)->ncol >= curcol && (*pp)->ncol < curcol + arg)
		    (*pp)->ncol = curcol;
		if ((*pp)->ncol >= curcol + arg)
		    ((*pp)->ncol) -= arg;
		if ((*pp)->nlastcol >= curcol && (*pp)->nlastcol < curcol + arg)
		    (*pp)->nlastcol = curcol - 1;
		if ((*pp)->nlastcol >= curcol + arg)
		    ((*pp)->nlastcol) -= arg;
		if ((*pp)->nlastcol < (*pp)->ncol)
		    (*pp)->nrow = (*pp)->ncol = -1;
	    }
	}
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];

    FullUpdate++;
    modflg++;
}

void
doend(int rowinc, int colinc)
{
    register struct ent *p;
    int r, c;

    if (!loading)
	remember(0);

    if (VALID_CELL(p, currow, curcol)) {
	r = currow + rowinc;
	c = curcol + colinc;
	if (r >= 0 && r < maxrows && 
	    c >= 0 && c < maxcols &&
	    !VALID_CELL(p, r, c)) {
		currow = r;
		curcol = c;
	}
    }

    if (!VALID_CELL(p, currow, curcol)) {
        switch (rowinc) {
        case -1:
	    while (!VALID_CELL(p, currow, curcol) && currow > 0)
		currow--;
	    break;
        case  1:
	    while (!VALID_CELL(p, currow, curcol) && currow < maxrows-1)
		currow++;
	    break;
        case  0:
            switch (colinc) {
 	    case -1:
	        while (!VALID_CELL(p, currow, curcol) && curcol > 0)
		    curcol--;
	        break;
 	    case  1:
	        while (!VALID_CELL(p, currow, curcol) && curcol < maxcols-1)
		    curcol++;
	        break;
	    }
            break;
        }
	rowsinrange = 1;
	colsinrange = fwidth[curcol];
	if (!loading)
	    remember(1);

	CLEAR_LINE;	/* clear line */
	return;
    }

    switch (rowinc) {
    case -1:
	while (VALID_CELL(p, currow, curcol) && currow > 0)
	    currow--;
	break;
    case  1:
	while (VALID_CELL(p, currow, curcol) && currow < maxrows-1)
	    currow++;
	break;
    case  0:
	switch (colinc) {
	case -1:
	    while (VALID_CELL(p, currow, curcol) && curcol > 0)
		curcol--;
	    break;
	case  1:
	    while (VALID_CELL(p, currow, curcol) && curcol < maxcols-1)
		curcol++;
	    break;
	}
	break;
    }
    if (!VALID_CELL(p, currow, curcol)) {
        currow -= rowinc;
        curcol -= colinc;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* Modified 9/17/90 THA to handle more formats */
void
doformat(int c1, int c2, int w, int p, int r)
{
    register int i;
    int crows = 0;
    int ccols = c2;

    if (c1 >= maxcols && !growtbl(GROWCOL, 0, c1)) c1 = maxcols-1 ;
    if (c2 >= maxcols && !growtbl(GROWCOL, 0, c2)) c2 = maxcols-1 ;
    
    if (w == 0) {
	error("Width too small - setting to 1");
	w = 1;
    }

    if (usecurses && w > COLS - rescol - 2) {
	error("Width too large - Maximum = %d", COLS - rescol - 2);
	w = COLS - rescol - 2;
    }

    if (p > w) {
	error("Precision too large");
	p = w;
    }

    checkbounds(&crows, &ccols);
    if (ccols < c2) {
	error("Format statement failed to create implied column %d", c2);
	return;
    }

    for (i = c1; i <= c2; i++)
	fwidth[i] = w, precision[i] = p, realfmt[i] = r;

    rowsinrange = 1;
    colsinrange = fwidth[curcol];

    FullUpdate++;
    modflg++;
}

void
formatcol(int arg) {
    int c, i;
    int mf = modflg;
    int *oldformat;

    error("Current format is %d %d %d",
	    fwidth[curcol], precision[curcol],
	    realfmt[curcol]);
    refresh();
    oldformat = scxmalloc(arg*3*sizeof(int));
    for (i = 0; i < arg; i++) {
	oldformat[i * 3 + 0] = fwidth[i + curcol];
	oldformat[i * 3 + 1] = precision[i + curcol];
	oldformat[i * 3 + 2] = realfmt[i + curcol];
    }
    c = nmgetch();
    while (c >= 0 && c != ctl('m') && c != 'q' && c != ESC &&
	    c != ctl('g') && linelim < 0) {
	if (c >= '0' && c <= '9')
	    for (i = curcol; i < curcol + arg; i++)
		realfmt[i] = c - '0';
	else
	    switch (c) {
		case KEY_LEFT:
		case '<':
		case 'h':
		    for (i = curcol; i < curcol + arg; i++) {
			fwidth[i]--;
			if (fwidth[i] < 1)
			    fwidth[i] = 1;
		    }
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    modflg++;
		    break;
		case KEY_RIGHT:
		case '>':
		case 'l':
		    for (i = curcol; i < curcol + arg; i++) {
			fwidth[i]++;
			if (fwidth[i] > COLS - rescol - 2)
			    fwidth[i] = COLS - rescol - 2;
		    }
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    modflg++;
		    break;
		case KEY_DOWN:
		case '-':
		case 'j':
		    for (i = curcol; i < curcol + arg; i++) {
			precision[i]--;
			if (precision[i] < 0)
			    precision[i] = 0;
		    }
		    modflg++;
		    break;
		case KEY_UP:
		case '+':
		case 'k':
		    for (i = curcol; i < curcol + arg; i++)
			precision[i]++;
		    modflg++;
		    break;
		case ' ':
		    if (arg == 1)
			snprintf(line, sizeof line,
				"format [for column] %s ",
				coltoa(curcol));
		    else {
			snprintf(line, sizeof line,
				"format [for columns] %s:",
				coltoa(curcol));
			linelim = strlen(line);
			snprintf(line + strlen(line), sizeof(line) - linelim,
			    "%s ", coltoa(curcol+arg-1));
		    }
		    linelim = strlen(line);
		    insert_mode();
		    error("Current format is %d %d %d",
			    fwidth[curcol], precision[curcol],
			    realfmt[curcol]);
		    continue;
		case '=':
		    error("Define format type (0-9):");
		    refresh();
		    if ((c = nmgetch()) >= '0' && c <= '9') {
			if (colformat[c-'0']) {
			    snprintf(line, sizeof line,
				    "format %c = \"%s\"", c, colformat[c-'0']);
			    edit_mode();
			    linelim = strlen(line) - 1;
			} else {
			    snprintf(line, sizeof line,
				    "format %c = \"", c);
			    insert_mode();
			    linelim = strlen(line);
			}
			CLEAR_LINE;
		    } else {
			error("Invalid format type");
			c = -1;
		    }
		    continue;
		case ctl('l'):
		    FullUpdate++;
		    clearok(stdscr, 1);
		    break;
		default:
		    break;
	    }
	error("Current format is %d %d %d",
		fwidth[curcol], precision[curcol],
		realfmt[curcol]);
	FullUpdate++;
	update(1);
	refresh();
	if (linelim < 0)
	    if ((c = nmgetch()) == ESC || c == ctl('g') || c == 'q') {
		for (i = 0; i < arg; i++) {
		    fwidth[i + curcol] = oldformat[i * 3 + 0];
		    precision[i + curcol] = oldformat[i * 3 + 1];
		    realfmt[i + curcol] = oldformat[i * 3 + 2];
		}
		modflg = mf;
		FullUpdate++;
		update(1);
	    }
    }
    scxfree((char *)oldformat);
    if (c >= 0)
	CLEAR_LINE;
}

void
ljustify(int sr, int sc, int er, int ec) {
    struct ent *p;
    int i, j;

    if (sr > er) {
	i = sr;
	sr = er;
	er = i;
    }
    if (sc > ec) {
	i = sc;
	sc = ec;
	ec = i;
    }
    for (i = sr; i <= er; i++) {
	for (j = sc; j <= ec; j++) {
	    p = *ATBL(tbl, i, j);
	    if (p && p->label) {
		p->flags &= ~IS_LABEL;
		p->flags |= IS_LEFTFLUSH | IS_CHANGED;
		changed++;
		modflg++;
	    }
	}
    }
}

void
rjustify(int sr, int sc, int er, int ec) {
    struct ent *p;
    int i, j;

    if (sr > er) {
	i = sr;
	sr = er;
	er = i;
    }
    if (sc > ec) {
	i = sc;
	sc = ec;
	ec = i;
    }
    for (i = sr; i <= er; i++) {
	for (j = sc; j <= ec; j++) {
	    p = *ATBL(tbl, i, j);
	    if (p && p->label) {
		p->flags &= ~(IS_LABEL | IS_LEFTFLUSH);
		p->flags |= IS_CHANGED;
		changed++;
		modflg++;
	    }
	}
    }
}

void
center(int sr, int sc, int er, int ec) {
    struct ent *p;
    int i, j;

    if (sr > er) {
	i = sr;
	sr = er;
	er = i;
    }
    if (sc > ec) {
	i = sc;
	sc = ec;
	ec = i;
    }
    for (i = sr; i <= er; i++) {
	for (j = sc; j <= ec; j++) {
	    p = *ATBL(tbl, i, j);
	    if (p && p->label) {
		p->flags &= ~IS_LEFTFLUSH;
		p->flags |= IS_LABEL | IS_CHANGED;
		changed++;
		modflg++;
	    }
	}
    }
}

void
print_options(FILE *f)
{
    if (
	autocalc &&
	!autoinsert &&
	!autowrap &&
	!cslop &&
	!optimize &&
	!rndtoeven &&
	propagation == 10 &&
	calc_order == BYROWS &&
	!numeric &&
	prescale == 1.0 &&
	!extfunc &&
	showtop &&
	tbl_style == 0 &&
	craction == 0 &&
	pagesize == 0 &&
	rowlimit == -1 &&
	collimit == -1 &&
	!color &&
	!colorneg &&
	!colorerr
       )
	return;		/* No reason to do this */

    (void) fprintf(f, "set");
    if (!autocalc) 
	(void) fprintf(f," !autocalc");
    if (autoinsert) 
	(void) fprintf(f," autoinsert");
    if (autowrap) 
	(void) fprintf(f," autowrap");
    if (cslop)
	(void) fprintf(f," cslop");
    if (optimize) 
	(void) fprintf(f," optimize");
    if (rndtoeven)
	(void) fprintf(f, " rndtoeven");
    if (propagation != 10)
	(void) fprintf(f, " iterations = %d", propagation);
    if (calc_order != BYROWS )
	(void) fprintf(f, " bycols");
    if (numeric)
	(void) fprintf(f, " numeric");
    if (prescale != 1.0)
	(void) fprintf(f, " prescale");
    if (extfunc)
	(void) fprintf(f, " extfun");
    if (!showtop)
	(void) fprintf(f, " !toprow");
    if (tbl_style)
	(void) fprintf(f, " tblstyle = %s", tbl_style == TBL ? "tbl" :
					tbl_style == LATEX ? "latex" :
					tbl_style == SLATEX ? "slatex" :
					tbl_style == TEX ? "tex" :
					tbl_style == FRAME ? "frame" : "0" );
    if (craction)
	(void) fprintf(f, " craction = %d", craction);
    if (pagesize)
	(void) fprintf(f, " pagesize = %d", pagesize);
    if (rowlimit >= 0)
	(void) fprintf(f, " rowlimit = %d", rowlimit);
    if (collimit >= 0)
	(void) fprintf(f, " collimit = %d", collimit);
    if (color) 
	(void) fprintf(f," color");
    if (colorneg) 
	(void) fprintf(f," colorneg");
    if (colorerr) 
	(void) fprintf(f," colorerr");
    (void) fprintf(f, "\n");
}

void
printfile(char *fname, int r0, int c0, int rn, int cn)
{
    FILE *f;
    static char *pline = NULL;		/* only malloc once, malloc is slow */
    static unsigned fbufs_allocated = 0;
    int plinelim;
    int pid;
    unsigned int fieldlen, nextcol;
    long namelen;
    int row, col;
    register struct ent **pp;
    size_t fnamesiz;
    char file[256];
    char path[1024];
    char *tpp;

    if (fname) {
	/* printfile will be the [path/]file ---> [path/]file.out */
	if (*fname == '\0') {
	    strlcpy(path, curfile, sizeof path);

#ifdef MSDOS
	    namelen = 12;
	    if ((tpp = strrchr(path, '\\')) == NULL) {
#else
	    if ((tpp = strrchr(path, '/')) == NULL) {
		namelen = pathconf(".", _PC_NAME_MAX);
#endif
		tpp = path;
	    } else {
#ifndef MSDOS
		*tpp = '\0';
		namelen = pathconf(path, _PC_NAME_MAX);
		*tpp = '/';
#endif
		tpp++;
	    }
	    strlcpy(file, tpp, sizeof file);

	    if (!strcmp(file + strlen(file) - 3, ".sc"))
		file[strlen(file) - 3] = '\0';
	    else if (scext != NULL && file[strlen(file) - strlen(scext) - 1] == '.'
		    && !strcmp(file + strlen(file) - strlen(scext), scext))
		file[strlen(file) - strlen(scext)] = '\0';

	    if (ascext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(ascext) - 1] = '\0';
	    snprintf(tpp, sizeof(path) - (tpp - path), "%s.%s", file,
	      ascext == NULL ? "asc" : ascext);
	    fname = path;
	    fnamesiz = sizeof path;
	} else
	    /* strarg in gram.y, always size of \0 terminated string. */
	    /* TODO: Possible problem if ~ needs to be expanded. */
	    fnamesiz = strlen(fname) + 1;

	if ((strcmp(fname, curfile) == 0) &&
	    !yn_ask("Confirm that you want to destroy the data base: (y,n)")) {
	    return;
	}

	if ((f = openfile(fname, fnamesiz,
	  &pid, NULL)) == (FILE *)0) {
	    error("Can't create file \"%s\"", fname);
	    return;
	}
    } else
	f = stdout;

    if (!pline && (pline = scxmalloc(FBUFLEN *
	    ++fbufs_allocated)) == (char *)NULL) {
	error("Malloc failed in printfile()");
	return;
    }

    for (row = r0; row <= rn; row++) {
	unsigned int c = 0;

	if (row_hidden[row])
	    continue;

	pline[plinelim=0] = '\0';
	for (pp = ATBL(tbl, row, col=c0); col<=cn;
	        pp += nextcol-col, col = nextcol, c += fieldlen) {

	    nextcol = col+1;
	    if (col_hidden[col]) {
		fieldlen = 0;
		continue;
	    }

	    fieldlen = fwidth[col];
	    if (*pp) {
		char *s;

		/* 
		 * dynamically allocate pline, making sure we are not 
		 * attempting to write 'out of bounds'.
		 */
		while (c > (fbufs_allocated * FBUFLEN)) {
		    if ((pline = scxrealloc (pline, 
			    FBUFLEN * ++fbufs_allocated)) == NULL) {
			error ("Realloc failed in printfile()");
			return;
		    }
		}		  
		while (plinelim < (int)c) pline[plinelim++] = ' ';
		plinelim = c;
		if ((*pp)->flags&IS_VALID) {
		    while(plinelim + fwidth[col] > 
			  (int)(fbufs_allocated * FBUFLEN)) {
		      if((pline = scxrealloc(pline, FBUFLEN *
		       ++fbufs_allocated)) == NULL) {
			error("Realloc failed in printfile()");
			return;
		      }
		    }
		    if ((*pp)->cellerror)
			snprintf(pline + plinelim,
			  FBUFLEN * fbufs_allocated - plinelim,
			  "%*s", fwidth[col], ((*pp)->cellerror == CELLERROR ?
			  "ERROR " : "INVALID "));
		    else {
		      char *cfmt;

		      cfmt = (*pp)->format ? (*pp)->format :
			    (realfmt[col] >= 0 && realfmt[col] < COLFORMATS &&
			    colformat[realfmt[col]]) ?
			    colformat[realfmt[col]] : 0;
		      if (cfmt) {
	   	        char field[FBUFLEN];

			if (*cfmt == ctl('d')) {
			    time_t v = (time_t) ((*pp)->v);
			    strftime(field, sizeof(field), cfmt + 1,
				    localtime(&v));
			    snprintf(pline + plinelim,
			      FBUFLEN * fbufs_allocated - plinelim,
			      "%-*s", fwidth[col], field);
			} else {
			    format(cfmt, precision[col], (*pp)->v, field,
				    sizeof(field));
			    snprintf(pline+plinelim,
			      FBUFLEN * fbufs_allocated - plinelim,
			      "%*s", fwidth[col], field);
			}
		      } else {
	   	        char field[FBUFLEN];
			engformat(realfmt[col], fwidth[col],
                                             precision[col], (*pp) -> v,
                                             field, sizeof(field));
			snprintf(pline+plinelim,
			  FBUFLEN * fbufs_allocated - plinelim,
			  "%*s", fwidth[col], field);
		      }
		    }
		    plinelim += strlen(pline+plinelim);
		}
		if ((s = (*pp)->label)) {
		    ssize_t slen;
		    char *start, *last;
		    register char *fp;
		    struct ent *nc;

		    /*
		     * Figure out if the label slops over to a blank field.
		     * A string started with backslash is defining repetition
		     * char
		     */
		    slen = strlen(s);
		    if (*s == '\\' && *(s+1) != '\0')
			slen = fwidth[col];
		    while (slen > (ssize_t)fieldlen && (int)nextcol <= cn &&
			    !((nc = lookat(row,nextcol))->flags & IS_VALID) &&
			    !(nc->label)) {
			
	                if (!col_hidden[nextcol])
		 	    fieldlen += fwidth[nextcol];

			nextcol++;
		    }
		    if (slen > (ssize_t)fieldlen)
			slen = fieldlen;
		    
		    while(c + fieldlen + 2 > (fbufs_allocated * FBUFLEN)) {
		      if((pline = scxrealloc(pline, FBUFLEN *
		       ++fbufs_allocated)) == NULL) {
			error ("scxrealloc failed in printfile()");
			return;
		      }
		    }

		    /* Now justify and print */
		    start = (*pp)->flags & IS_LEFTFLUSH ? pline + c
					: pline + c + fieldlen - slen;
		    if( (*pp)->flags & IS_LABEL )
			start = pline + (c + ((fwidth[col] > slen) ?
			  (fwidth[col] - slen) / 2 : 0));
		    last = pline + c + fieldlen;
		    fp = plinelim < (int)c ? pline + plinelim : pline + c;
		    while (fp < start)
			*fp++ = ' ';
		    if( *s == '\\' && *(s+1)!= '\0' ) {
			char *strt;
			strt = ++s;

			while(slen--) {
				*fp++ = *s++;
				if (*s == '\0')
					s = strt;
			}
		    } else {
			while (slen-- > 0) {
			    if (*s == '\\') {
				if (s[1] == '"') {
				    ++s;
				    --slen;
				} else if (s[1] == '\\' && s[2] == '"') {
				    s    += 2;
				    slen -= 2;
				}
			    }
			    *fp++ = *s++;
			}
		    }

		    if (!((*pp)->flags & IS_VALID) ||
		      (int)fieldlen != fwidth[col])
			while(fp < last)
			    *fp++ = ' ';
		    if (plinelim < fp - pline)
			plinelim = fp - pline;
		}
	    }
	}
	pline[plinelim++] = '\n';
	pline[plinelim] = '\0';
	(void) fputs (pline, f);
    }

    if (fname) closefile(f, pid, 0);
}

void
tblprintfile(char *fname, int r0, int c0, int rn, int cn)
{
    FILE *f;
    int pid;
    long namelen;
    int row, col;
    register struct ent **pp;
    size_t fnamesiz;
    char coldelim = DEFCOLDELIM;
    char file[256];
    char path[1024];
    char *tpp;

    /* tblprintfile will be the [path/]file ---> [path/]file.out */
    if (*fname == '\0') {
	strlcpy(path, curfile, sizeof path);

#ifdef MSDOS
	namelen = 12;
	if ((tpp = strrchr(path, '\\')) == NULL) {
#else
	if ((tpp = strrchr(path, '/')) == NULL) {
	    namelen = pathconf(".", _PC_NAME_MAX);
#endif
	    tpp = path;
	} else {
#ifndef MSDOS
	    *tpp = '\0';
	    namelen = pathconf(path, _PC_NAME_MAX);
	    *tpp = '/';
#endif
	    tpp++;
	}
	strlcpy(file, tpp, sizeof file);

	if (!strcmp(file + strlen(file) - 3, ".sc"))
	    file[strlen(file) - 3] = '\0';
	else if (scext != NULL && file[strlen(file) - strlen(scext) - 1] == '.'
		&& !strcmp(file + strlen(file) - strlen(scext), scext))
	    file[strlen(file) - strlen(scext)] = '\0';

	if (tbl_style == 0) {
	    if (tbl0ext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(tbl0ext) - 1] = '\0';
	    snprintf(tpp, sizeof(path) - (tpp - path), "%s.%s", file,
	      tbl0ext == NULL ? "cln" : tbl0ext);
	}
	else if (tbl_style == TBL) {
	    if (tblext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(tblext) - 1] = '\0';
	    snprintf(tpp, sizeof(path) - (tpp - path), "%s.%s", file,
	      tblext == NULL ? "tbl" : tblext);
	}
	else if (tbl_style == LATEX) {
	    if (latexext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(latexext) - 1] = '\0';
	    snprintf(tpp, sizeof(path) - (tpp - path), "%s.%s", file,
	      latexext == NULL ? "lat" : latexext);
	}
	else if (tbl_style == SLATEX) {
	    if (slatexext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(slatexext) - 1] = '\0';
	    snprintf(tpp, sizeof(path) - (tpp - path), "%s.%s", file,
	      slatexext == NULL ? "stx" : slatexext);
	}
	else if (tbl_style == TEX) {
	    if (texext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(texext) - 1] = '\0';
	    snprintf(tpp, sizeof(path) - (tpp - path), "%s.%s", file,
	      texext == NULL ? "tex" : texext);
	}
	fname = path;
	fnamesiz = sizeof path;
    } else
	/* TODO: Possible problem if ~ needs to be expanded. */
	fnamesiz = strlen(fname) + 1;

    if ((strcmp(fname, curfile) == 0) &&
	!yn_ask("Confirm that you want to destroy the data base: (y,n)"))
	    return;

    if ((f = openfile(fname, fnamesiz,
      &pid, NULL)) == (FILE *)0) {
	error ("Can't create file \"%s\"", fname);
	return;
    }

    if ( tbl_style == TBL ) {
	fprintf(f,".\\\" ** %s spreadsheet output \n.TS\n",progname);
	fprintf(f,"tab(%c);\n",coldelim);
	for (col=c0;col<=cn; col++) fprintf(f," n");
	fprintf(f, ".\n");
    }
    else if ( tbl_style == LATEX ) {
	fprintf(f,"%% ** %s spreadsheet output\n\\begin{tabular}{",progname);
	for (col=c0;col<=cn; col++) fprintf(f,"c");
	fprintf(f, "}\n");
	coldelim = '&';
    }
    else if ( tbl_style == SLATEX ) {
	fprintf(f,"%% ** %s spreadsheet output\n!begin<tabular><",progname);
	for (col=c0;col<=cn; col++) fprintf(f,"c");
	fprintf(f, ">\n");
	coldelim = '&';
    }
    else if ( tbl_style == TEX ) {
	fprintf(f,"{\t%% ** %s spreadsheet output\n\\settabs %d \\columns\n",
		progname, cn-c0+1);
	coldelim = '&';
    }
    else if ( tbl_style == FRAME ) {
	fprintf(f,"<MIFFile 3.00> # generated by the sc spreadsheet calculator\n");
	fprintf(f,"<Tbls\n");
	fprintf(f," <Tbl \n");
	fprintf(f,"  <TblID 1> # This table's ID is 1\n");
	fprintf(f,"  <TblFormat \n");
	fprintf(f,"   <TblTag `Format A'> # Table Format Catalog\n");
	fprintf(f,"  > # end of TblFormat\n");
	fprintf(f,"  <TblNumColumns %d> # Has %d columns\n",cn-c0+1,cn-c0+1);
	fprintf(f,"  <TblTitleContent\n");
	fprintf(f,"   <Para\n");
	fprintf(f,"    <PgfTag `TableTitle'> # Forces lookup in Paragraph Format Catalog\n");
	fprintf(f,"    <ParaLine\n");
	fprintf(f,"     <String `%s'>\n",fname);
	fprintf(f,"    > # end of ParaLine\n");
	fprintf(f,"   > # end of Para\n");
	fprintf(f,"  > # end of TblTitleContent\n");
	fprintf(f,"  <TblH # The heading\n");
	fprintf(f,"   <Row # The heading row\n");
	for (col=c0; col <= cn; col++) {
	    fprintf(f,"    <Cell <CellContent <Para # Cell in column \n");
	    fprintf(f,"       <PgfTag `CellHeading'> # in Paragraph Format Catalog\n");
	    fprintf(f,"       <ParaLine <String `%c'>>\n",'A'+col);
	    fprintf(f,"    >>> # end of Cell\n");
	}
	fprintf(f,"   > # end of Row\n");
	fprintf(f,"  > # end of TblH\n");
	fprintf(f,"  <TblBody # The body\n");
    }

    for (row=r0; row<=rn; row++) {
	if ( tbl_style == TEX )
	    (void) fprintf (f, "\\+");
	else if (tbl_style == FRAME) {
	    fprintf(f,"   <Row # The next body row\n");
	}
	
	for (pp = ATBL(tbl, row, col=c0); col<=cn; col++, pp++) {
	    if ( tbl_style == FRAME ) {
		fprintf(f,"    <Cell <CellContent <Para\n");
		fprintf(f,"       <PgfTag `CellBody'> # in Paragraph Format Catalog\n");
		fprintf(f,"       <ParaLine <String `");
	    } 
	    if (*pp) {
		char *s;
		if ((*pp)->flags&IS_VALID) {
		    if ((*pp)->cellerror) {
			(void) fprintf (f, "%*s",
					fwidth[col],
			((*pp)->cellerror == CELLERROR ? "ERROR" : "INVALID"));
		    } else if ((*pp)->format) {
		        char field[FBUFLEN];
			if (*((*pp)->format) == ctl('d')) {
			    time_t v = (time_t) ((*pp)->v);
			    strftime(field, sizeof(field), ((*pp)->format)+1,
				    localtime(&v));
			} else
			    format((*pp)->format, precision[col], (*pp)->v,
				    field, sizeof(field));
			unspecial(f, field, coldelim);
		    } else {
		        char field[FBUFLEN];
                        (void) engformat(realfmt[col], fwidth[col],
				precision[col], (*pp) -> v,
				field, sizeof(field));
			unspecial(f, field, coldelim);
		    }
		}
		if ((s = (*pp)->label)) {
	            unspecial(f, s, coldelim);
		}
	    }
	    if (tbl_style == FRAME) {
		fprintf(f, "'>>\n");
		fprintf(f,"    >>> # end of Cell\n");
	    }
	    if (col < cn)
		if (tbl_style != FRAME)
		    (void) fprintf(f,"%c", coldelim);
	}
	if (tbl_style == LATEX) {
		if (row < rn) (void) fprintf (f, "\\\\");
	}
	else if (tbl_style == SLATEX) {
		if (row < rn) (void) fprintf(f, "!!");
	}
	else if (tbl_style == TEX) {
		(void) fprintf (f, "\\cr");
	}
	else if (tbl_style == FRAME) {
	    fprintf(f,"   > # end of Row\n");
	}
	(void) fprintf(f,"\n");
    }

    if (tbl_style == TBL)
    (void) fprintf (f,".TE\n.\\\" ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == LATEX)
	(void) fprintf(f,"\\end{tabular}\n%% ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == SLATEX)
	(void) fprintf (f,"!end<tabular>\n%% ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == TEX)
	(void) fprintf (f,"}\n%% ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == FRAME) {
	fprintf(f,"  > # end of TblBody\n");
	fprintf(f," ># end of Tbl\n");
	fprintf(f,"> # end of Tbls\n");
	fprintf(f,"<TextFlow <Para \n");
	fprintf(f,"  <PgfTag Body> \n");
	fprintf(f,"  <ParaLine <ATbl 1>> # Reference to table ID 1\n");
	fprintf(f,">>\n");
    }

    closefile(f, pid, 0);
}

/* unspecial (backquote) things that are special chars in a table */
void
unspecial(FILE *f, char *str, int delim)
{
    if (*str == '\\') str++; /* delete wheeling string operator, OK? */
    while (*str) {
    	if (((tbl_style == LATEX) || (tbl_style == SLATEX) ||
		(tbl_style == TEX)) &&
		((*str == delim) || (*str == '$') || (*str == '#') ||
		(*str == '%') || (*str == '{') || (*str == '}') ||
		(*str == '&')))
	    putc('\\', f);
	putc(*str, f);
	str++;
    }
}

struct enode *
copye(register struct enode *e, int Rdelta, int Cdelta, int r1, int c1,
	int r2, int c2, int transpose)
{
    register struct enode *ret;
    static struct enode *range = NULL;

    if (e == (struct enode *)0)
	ret = (struct enode *)0;
    else if (e->op & REDUCE) {
	int newrow, newcol;
	if (freeenodes) {
	    ret = freeenodes;
	    freeenodes = ret->e.o.left;
	} else
	    ret = scxmalloc(sizeof (struct enode));
	ret->op = e->op;
	newrow = e->e.r.left.vf & FIX_ROW ||
		 e->e.r.left.vp->row < r1 || e->e.r.left.vp->row > r2 ||
		 e->e.r.left.vp->col < c1 || e->e.r.left.vp->col > c2 ?
		 e->e.r.left.vp->row :
		 transpose ? r1 + Rdelta + e->e.r.left.vp->col - c1 :
		 e->e.r.left.vp->row + Rdelta;
	newcol = e->e.r.left.vf & FIX_COL ||
		 e->e.r.left.vp->row < r1 || e->e.r.left.vp->row > r2 ||
		 e->e.r.left.vp->col < c1 || e->e.r.left.vp->col > c2 ?
		 e->e.r.left.vp->col :
		 transpose ? c1 + Cdelta + e->e.r.left.vp->row - r1 :
		 e->e.r.left.vp->col + Cdelta;
	ret->e.r.left.vp = lookat(newrow, newcol);
	ret->e.r.left.vf = e->e.r.left.vf;
	newrow = e->e.r.right.vf & FIX_ROW ||
		 e->e.r.right.vp->row < r1 || e->e.r.right.vp->row > r2 ||
		 e->e.r.right.vp->col < c1 || e->e.r.right.vp->col > c2 ?
		 e->e.r.right.vp->row :
		 transpose ? r1 + Rdelta + e->e.r.right.vp->col - c1 :
		 e->e.r.right.vp->row + Rdelta;
	newcol = e->e.r.right.vf & FIX_COL ||
		 e->e.r.right.vp->row < r1 || e->e.r.right.vp->row > r2 ||
		 e->e.r.right.vp->col < c1 || e->e.r.right.vp->col > c2 ?
		 e->e.r.right.vp->col :
		 transpose ? c1 + Cdelta + e->e.r.right.vp->row - r1 :
		 e->e.r.right.vp->col + Cdelta;
	ret->e.r.right.vp = lookat(newrow, newcol);
	ret->e.r.right.vf = e->e.r.right.vf;
    } else {
	struct enode *temprange = NULL;
	size_t l;

	if (freeenodes) {
	    ret = freeenodes;
	    freeenodes = ret->e.o.left;
	} else
	    ret = scxmalloc(sizeof (struct enode));
	ret->op = e->op;
	switch (ret->op) {
	    case SUM:
	    case PROD:
	    case AVG:
	    case COUNT:
	    case STDDEV:
	    case MAX:
	    case MIN:
		temprange = range;
		range = e->e.o.left;
		r1 = 0;
		c1 = 0;
		r2 = maxrow;
		c2 = maxcol;
	}
	switch (ret->op) {
	    case 'v':
		{
		    int newrow, newcol;
		    if (range && e->e.v.vp->row >= range->e.r.left.vp->row &&
				 e->e.v.vp->row <= range->e.r.right.vp->row &&
				 e->e.v.vp->col >= range->e.r.left.vp->col &&
				 e->e.v.vp->col <= range->e.r.right.vp->col) {
			newrow = range->e.r.left.vf & FIX_ROW ?
				e->e.v.vp->row : e->e.v.vp->row + Rdelta;
			newcol = range->e.r.left.vf & FIX_COL ?
				e->e.v.vp->col : e->e.v.vp->col + Cdelta;
		    } else {
			newrow = e->e.v.vf & FIX_ROW ||
				 e->e.v.vp->row < r1 || e->e.v.vp->row > r2 ||
				 e->e.v.vp->col < c1 || e->e.v.vp->col > c2 ?
				 e->e.v.vp->row :
				 transpose ? r1 + Rdelta + e->e.v.vp->col - c1 :
				 e->e.v.vp->row + Rdelta;
			newcol = e->e.v.vf & FIX_COL ||
				 e->e.v.vp->row < r1 || e->e.v.vp->row > r2 ||
				 e->e.v.vp->col < c1 || e->e.v.vp->col > c2 ?
				 e->e.v.vp->col :
				 transpose ? c1 + Cdelta + e->e.v.vp->row - r1 :
				 e->e.v.vp->col + Cdelta;
		    }
		    ret->e.v.vp = lookat(newrow, newcol);
		    ret->e.v.vf = e->e.v.vf;
		    break;
		}
	    case 'k':
		ret->e.k = e->e.k;
		break;
	    case 'f':
	    case 'F':
		if (( range && ret->op == 'F') ||
		    (!range && ret->op == 'f')   )
		    Rdelta = Cdelta = 0;
		ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
			r1, c1, r2, c2, transpose);
		ret->e.o.right = (struct enode *)0;
 		break;
	    case '$':
	    case EXT:
		l = strlen(e->e.s) + 1;
		ret->e.s = scxmalloc(l);
		strlcpy(ret->e.s, e->e.s, l);
		if (e->op == '$')	/* Drop through if ret->op is EXT */
		    break;
	    default:
		ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
			r1, c1, r2, c2, transpose);
		ret->e.o.right = copye(e->e.o.right, Rdelta, Cdelta,
			r1, c1, r2, c2, transpose);
		break;
	}
	switch (ret->op) {
	    case SUM:
	    case PROD:
	    case AVG:
	    case COUNT:
	    case STDDEV:
	    case MAX:
	    case MIN:
		range = temprange;
	}
    }
    return ret;
}

/*
 * sync_refs and syncref are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
void
sync_refs(void)
{
    int i, j;
    register struct ent *p;
    sync_ranges();
    for (i=0; i<=maxrow; i++)
	for (j=0; j<=maxcol; j++)
	    if ((p = *ATBL(tbl, i, j)) && p->expr)
		syncref(p->expr);
    for (i = 0; i < DELBUFSIZE; i++) {
	p = delbuf[i];
	while (p && p->expr) {
	    syncref(p->expr);
	    p = p->next;
	}
    }
}

void
syncref(register struct enode *e)
{
    if (e == (struct enode *)0)
	return;
    else if (e->op & REDUCE) {
 	e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
 	e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
    } else {
	switch (e->op) {
	    case 'v':
		if (e->e.v.vp->flags & IS_CLEARED) {
		    e->op = ERR_;
		    e->e.o.left = NULL;
		    e->e.o.right = NULL;
		} else if (e->e.v.vp->flags & MAY_SYNC)
		    e->e.v.vp = lookat(e->e.v.vp->row, e->e.v.vp->col);
		break;
	    case 'k':
		break;
	    case '$':
		break;
	    default:
		syncref(e->e.o.right);
		syncref(e->e.o.left);
		break;
	}
    }
}

/* mark a row as hidden */
void
hiderow(int arg)
{
    register int r1;
    register int r2;

    r1 = currow;
    r2 = r1 + arg - 1;
    if (r1 < 0 || r1 > r2) {
	error("Invalid range");
	return;
    }
    if (r2 >= maxrows-1) {
    	if (!growtbl(GROWROW, arg+1, 0)) {
	    error("You can't hide the last row");
	    return;
	}
    }
    FullUpdate++;
    modflg++;
    while (r1 <= r2)
	row_hidden[r1++] = 1;
}

/* mark a column as hidden */
void
hidecol(int arg)
{
    int c1;
    int c2;

    c1 = curcol;
    c2 = c1 + arg - 1;
    if (c1 < 0 || c1 > c2) {
	error ("Invalid range");
	return;
    }
    if (c2 >= maxcols-1) {
    	if ((arg >= ABSMAXCOLS-1) || !growtbl(GROWCOL, 0, arg+1)) {
	    error("You can't hide the last col");
	    return;
	}
    }
    FullUpdate++;
    modflg++;
    while (c1 <= c2)
	col_hidden[c1++] = TRUE;
}

/* mark a row as not-hidden */
void
showrow(int r1, int r2)
{
    if (r1 < 0 || r1 > r2) {
	error ("Invalid range");
	return;
    }
    if (r2 > maxrows-1) {
	r2 = maxrows-1;
    }
    FullUpdate++;
    modflg++;
    while (r1 <= r2)
	row_hidden[r1++] = 0;
}

/* mark a column as not-hidden */
void
showcol(int c1, int c2)
{
    if (c1 < 0 || c1 > c2) {
	error ("Invalid range");
	return;
    }
    if (c2 > maxcols-1) {
	c2 = maxcols-1;
    }
    FullUpdate++;
    modflg++;
    while (c1 <= c2)
	col_hidden[c1++] = FALSE;
}

/* Open the input or output file, setting up a pipe if needed */
FILE *
openfile(char *fname, size_t fnamesiz, int *rpid, int *rfd)
{
    int pipefd[4];
    int pid;
    FILE *f;
    char *efname;

    while (*fname && (*fname == ' ')) {	/* Skip leading blanks */
	fname++;
	fnamesiz--;
    }

    if (*fname != '|') {		/* Open file if not pipe */
	*rpid = 0;
	if (rfd != NULL)
	    *rfd = 1;			/* Set to stdout just in case */
	
	efname = findhome(fname, fnamesiz);
	if (dobackups && rfd == NULL && !backup_file(efname) &&
	    (yn_ask("Could not create backup copy.  Save anyway?: (y,n)") != 1))
		return (0);
	return (fopen(efname, rfd == NULL ? "w" : "r"));
    }

#if defined(MSDOS)
    error("Piping not available under MS-DOS\n");
    return (0);
#else
    fname++;				/* Skip | */
    fnamesiz--;
    efname = findhome(fname, fnamesiz);
    if (pipe(pipefd) < 0 || (rfd != NULL && pipe(pipefd+2) < 0)) {
	error("Can't make pipe to child");
	*rpid = 0;
	return (0);
    }

    deraw(rfd==NULL);
#ifdef VMS
    fprintf(stderr, "No son tasks available yet under VMS--sorry\n");
#else /* VMS */

    if ((pid=fork()) == 0) {		/* if child */
	(void) close(0);		/* close stdin */
	(void) close(pipefd[1]);
	(void) dup(pipefd[0]);		/* connect to first pipe */
	if (rfd != NULL) {		/* if opening for read */
	    (void) close(1);		/* close stdout */
	    (void) close(pipefd[2]);
	    (void) dup(pipefd[3]);	/* connect to second pipe */
	}
	(void) signal(SIGINT, SIG_DFL);	/* reset */
	execl("/bin/sh", "sh", "-c", efname, (char *)NULL);
	exit (-127);
    } else {				/* else parent */
	*rpid = pid;
	if ((f = fdopen(pipefd[(rfd==NULL?1:2)], rfd==NULL?"w":"r")) == NULL) {
	    (void) kill(pid, 9);
	    error("Can't fdopen %sput", rfd==NULL?"out":"in");
	    (void) close(pipefd[1]);
	    if (rfd != NULL)
		(void) close(pipefd[3]);
	    *rpid = 0;
	    return (0);
	}
    }
    (void) close(pipefd[0]);
    if (rfd != NULL) {
	(void) close(pipefd[3]);
	*rfd = pipefd[1];
    }
#endif /* VMS */
    return (f);
#endif /* MSDOS */
}

/* close a file opened by openfile(), if process wait for return */
void
closefile(FILE *f, int pid, int rfd)
{
    int temp;

	if (fclose(f) == EOF) {
		error("fclose(): %s", strerror(errno));
	}
#ifndef MSDOS
    if (pid) {
	while (pid != wait(&temp)) /**/;
	if (rfd==0) {
	    (void) printf("Press any key to continue ");
	    (void) fflush(stdout);
	    cbreak();
	    (void) nmgetch();
	    goraw();
	    clear();
	} else {
	    close(rfd);
	    if (usecurses) {
# ifdef VMS
		VMS_read_raw = 1;
# else /* VMS */
#  ifdef HAVE_FIXTERM
		fixterm();
#  else
		cbreak();
		nonl();
		noecho ();
#  endif
		kbd_again();
# endif /* VMS */
		if (color && has_colors())
		    bkgdset(COLOR_PAIR(1) | ' ');
	    }
	}
    }
#endif /* MSDOS */
    if (brokenpipe) {
	error("Broken pipe");
	brokenpipe = FALSE;
    }
}

/* Copy a cell (struct ent).  "special" indicates special treatment when
 * merging two cells for the "pm" command, merging formats only for the
 * "pf" command, or for adjusting cell references when transposing with
 * the "pt" command.  r1, c1, r2, and c2 define the range in which the dr
 * and dc values should be used.
 */
void
copyent(register struct ent *n, register struct ent *p, int dr, int dc,
	int r1, int c1, int r2, int c2, int special)
{
    size_t l;
    if (!n || !p) {
	error("internal error");
	return;
    }
    if (special != 'f') {
	if (special != 'm' || p->flags & IS_VALID) {
	    n->v = p->v;
	    n->flags |= p->flags & IS_VALID;
	}
	if (special != 'm' || p->expr) {
	    n->expr = copye(p->expr, dr, dc, r1, c1, r2, c2, special == 't');
	    if (p->flags & IS_STREXPR)
		n->flags |= IS_STREXPR;
	    else
		n->flags &= ~IS_STREXPR;
	}
	if (p->label) {
	    if (n->label)
		scxfree(n->label);
	    l = strlen(p->label) + 1;
	    n->label = scxmalloc(l);
	    strlcpy(n->label, p->label, l);
	    n->flags &= ~IS_LEFTFLUSH;
	    n->flags |= ((p->flags & IS_LABEL) | (p->flags & IS_LEFTFLUSH));
	} else if (special != 'm') {
	    n->label = NULL;
	    n->flags &= ~(IS_LABEL | IS_LEFTFLUSH);
	}
	n->flags |= p->flags & IS_LOCKED;
    }
    if (p->format) {
	if (n->format)
	    scxfree(n->format);
	l = strlen(p->format) + 1;
        n->format = scxmalloc(l);
	strlcpy(n->format, p->format, l);
    } else if (special != 'm' && special != 'f')
	n->format = NULL;
    n->flags |= IS_CHANGED;
}

#ifndef MSDOS
/* add a plugin/mapping pair to the end of the filter list. type is
 * r(ead) or w(rite)
 */

void
addplugin(char *ext, char *plugin, char type)
{
    struct impexfilt *fp;
    char mesg[PATHLEN];

    if (!plugin_exists(plugin, strlen(plugin), mesg)) {
	error("Cannot find plugin %s", plugin);
	return;
    }
    if (filt == NULL) {
	filt = scxmalloc(sizeof(struct impexfilt));
	fp = filt;
    } else {
	fp = filt;
	while (fp->next != NULL)
	    fp = fp->next;
	fp->next = scxmalloc(sizeof(struct impexfilt));
	fp = fp->next;
    }
    strlcpy(fp->plugin, plugin, PATHLEN);
    strlcpy(fp->ext, ext, PATHLEN);
    fp->type = type;
    fp->next = NULL;
}

char *
findplugin(char *ext, char type)
{
    struct impexfilt *fp;

    fp = filt;
    if (fp == NULL)
	return (NULL);
    if ((!strcmp(fp->ext, ext)) && (fp->type == type))
	return (fp->plugin);
    while (fp->next != NULL) {
	fp = fp->next;
	if ((!strcmp(fp->ext, ext)) && (fp->type == type))
	    return (fp->plugin);
    }

    return (NULL);
}
#endif

void
write_fd(register FILE *f, int r0, int c0, int rn, int cn)
{
    register struct ent **pp;
    int r, c;

    (void) fprintf(f, "# This data file was generated by the Spreadsheet ");
    (void) fprintf(f, "Calculator.\n");
    (void) fprintf(f, "# You almost certainly shouldn't edit it.\n\n");
    print_options(f);
    for (c = 0; c < COLFORMATS; c++)
	if (colformat[c])
	    (void) fprintf (f, "format %d = \"%s\"\n", c, colformat[c]);
    for (c = c0; c <= cn; c++)
	if (fwidth[c] != DEFWIDTH || precision[c] != DEFPREC ||
		realfmt[c] != DEFREFMT)
	    (void) fprintf (f, "format %s %d %d %d\n", coltoa(c), fwidth[c],
		    precision[c], realfmt[c]);
    for (c = c0; c <= cn; c++)
        if (col_hidden[c])
            (void) fprintf(f, "hide %s\n", coltoa(c));
    for (r = r0; r <= rn; r++)
	if (row_hidden[r])
	    (void) fprintf(f, "hide %d\n", r);

    write_ranges(f);
    write_franges(f);
    write_colors(f, 0);
    write_cranges(f);

    if (mdir) 
	(void) fprintf(f, "mdir \"%s\"\n", mdir);
    if (autorun) 
	(void) fprintf(f, "autorun \"%s\"\n", autorun);
    for (c = 0; c < FKEYS; c++)
	if (fkey[c])
	    (void) fprintf(f, "fkey %d = \"%s\"\n", c + 1, fkey[c]);

    write_cells(f, r0, c0, rn, cn, r0, c0);
    for (r = r0; r <= rn; r++) {
	pp = ATBL(tbl, r, c0);
	for (c = c0; c <= cn; c++, pp++)
	    if (*pp) {
		if ((*pp)->flags&IS_LOCKED)
		    (void) fprintf(f, "lock %s%d\n", coltoa((*pp)->col),
						     (*pp)->row);
		if ((*pp)->nrow >= 0) {
		    (void) fprintf(f, "addnote %s ",
			    v_name((*pp)->row, (*pp)->col));
		    (void) fprintf(f, "%s\n", r_name((*pp)->nrow, (*pp)->ncol,
			    (*pp)->nlastrow, (*pp)->nlastcol));
		}
	    }
    }
    /*
     * Don't try to combine these into a single fprintf().  v_name() has
     * a single buffer that is overwritten on each call, so the first part
     * needs to be written to the file before making the second call.
     */
    fprintf(f, "goto %s", v_name(currow, curcol));
    fprintf(f, " %s\n", v_name(strow, stcol));
}

void
write_cells(register FILE *f, int r0, int c0, int rn, int cn, int dr, int dc)
{
    register struct ent **pp;
    int r, c, mf;
    int rs = 0;
    int cs = 0;
    char *dpointptr;

    mf = modflg;
    if (dr != r0 || dc != c0) {
	yank_area(r0, c0, rn, cn);
	rn += dr - r0;
	cn += dc - c0;
	rs = currow;
	cs = curcol;
	currow = dr;
	curcol = dc;
	pullcells('x');
    }
    if (Vopt) valueize_area(dr, dc, rn, cn);
    for (r = dr; r <= rn; r++) {
	pp = ATBL(tbl, r, dc);
	for (c = dc; c <= cn; c++, pp++)
	    if (*pp) {
		if ((*pp)->label || (*pp)->flags & IS_STREXPR) {
		    edits(r, c);
		    (void) fprintf(f, "%s\n", line);
		}
		if ((*pp)->flags & IS_VALID) {
		    editv(r, c);
		    dpointptr = strchr(line, dpoint);
		    if (dpointptr != NULL)
			*dpointptr = '.';
		    (void) fprintf(f, "%s\n", line);
		}
		if ((*pp)->format) {
		    editfmt(r, c);
		    (void) fprintf(f, "%s\n",line);
		}
	    }
    }
    if (dr != r0 || dc != c0) {
	pullcells('x');
	currow = rs;
	curcol = cs;
	flush_saved();
    }
    modflg = mf;
}

int
writefile(char *fname, int r0, int c0, int rn, int cn)
{
    register FILE *f;
    char save[PATHLEN];
    char tfname[PATHLEN];
    long namelen;
    char *tpp;
    char *p;
    char *plugin;
    int pid;

#ifndef MSDOS
    /* find the extension and mapped plugin if exists */
    if ((p = strrchr(fname, '.'))) {
	if ((plugin = findplugin(p+1, 'w')) != NULL) {
	    size_t l;
	    if (!plugin_exists(plugin, strlen(plugin), save + 1)) {
		error("plugin not found");
		return -1;
	    }
	    *save = '|';
	    if ((strlen(save) + strlen(fname) + 20) > PATHLEN) {
		error("Path too long");
		return -1;
	    }
	    l = strlen(save);
	    snprintf(save + l, sizeof(save) - l, " %s%d:", coltoa(c0), r0);
	    l = strlen(save);
	    snprintf(save + l, sizeof(save) - l, "%s%d \"%s\"", coltoa(cn),
		rn, fname);
	    /* pass it to readfile as an advanced macro */
	    readfile(save, 0);
	    return (0);
	}
    }
#endif

#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
    if (Crypt) {
	return (cwritefile(fname, r0, c0, rn, cn));
    }
#endif /* VMS */

    if (*fname == '\0') {
	if (isatty(STDOUT_FILENO) || *curfile != '\0')
	    fname = curfile;
	else {
	    write_fd(stdout, r0, c0, rn, cn);
	    return (0);
	}
    }

#ifdef MSDOS
    namelen = 12;
#else
    if ((tpp = strrchr(fname, '/')) == NULL)
	namelen = pathconf(".", _PC_NAME_MAX);
    else {
	*tpp = '\0';
	namelen = pathconf(fname, _PC_NAME_MAX);
	*tpp = '/';
    }
#endif /* MSDOS */

    (void) strlcpy(tfname, fname, sizeof tfname);
    for (tpp = tfname; *tpp != '\0'; tpp++)
	if (*tpp == '\\' && *(tpp + 1) == '"')
	    (void) memmove(tpp, tpp + 1, strlen(tpp));

    if (scext != NULL) {
	if (strlen(tfname) > 3 && !strcmp(tfname + strlen(tfname) - 3, ".sc"))
	    tfname[strlen(tfname) - 3] = '\0';
	else if (strlen(tfname) > strlen(scext) + 1 &&
		tfname[strlen(tfname) - strlen(scext) - 1] == '.' &&
		!strcmp(tfname + strlen(tfname) - strlen(scext), scext))
	    tfname[strlen(tfname) - strlen(scext) - 1] = '\0';
	tfname[namelen - strlen(scext) - 1] = '\0';
	strlcat(tfname, ".", sizeof tfname);
	strlcat(tfname, scext, sizeof tfname);
    }

    (void) strlcpy(save, tfname, sizeof save);
    for (tpp = save; *tpp != '\0'; tpp++)
	if (*tpp == '"') {
	    (void) memmove(tpp + 1, tpp, strlen(tpp) + 1);
	    *tpp++ = '\\';
	}

    if ((f = openfile(tfname, sizeof tfname, &pid, NULL)) == NULL) {
	error("Can't create file \"%s\"", save);
	return (-1);
    }

    if (usecurses) {
	error("Writing file \"%s\"...", save);
	refresh();
    }
    write_fd(f, r0, c0, rn, cn);
    
    closefile(f, pid, 0);

    if (!pid) {
        (void) strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
	FullUpdate++;
	if (usecurses) {
	    error("File \"%s\" written", curfile);
	} else
	    fprintf(stderr, "\nFile \"%s\" written", curfile);
    }

    return (0);
}

int
readfile(char *fname, int eraseflg)
{
    register FILE *f;
    char save[PATHLEN];
    int tempautolabel;
    char *p;
    char *plugin;
    int pid = 0;
    int rfd = STDOUT_FILENO, savefd;

    tempautolabel = autolabel;		/* turn off auto label when */
    autolabel = 0;			/* reading a file */

    if (*fname == '*' && mdir) { 
       (void) strlcpy(save, mdir, sizeof save);
       (void) strlcat(save, fname, sizeof save);
    } else {
        if (*fname == '\0')
	    fname = curfile;
	(void) strlcpy(save, fname, sizeof save);
    }

#ifndef MSDOS
    if ((p = strrchr(fname, '.')) && (fname[0] != '|')) {  /* exclude macros */
	if ((plugin = findplugin(p+1, 'r')) != NULL) {
	    size_t l;
	    if (!(plugin_exists(plugin, strlen(plugin), save + 1))) {
		error("plugin not found");
		return 0;
	    }
	    *save = '|';
	    if ((strlen(save) + strlen(fname) + 2) > PATHLEN) {
		error("Path too long");
		return 0;
	    }
	    l = strlen(save);
	    snprintf(save + l, sizeof(save) - l, " \"%s\"", fname);
	    eraseflg = 0;
	    /* get filename: could be preceded by params if this is
	    * a save
	    */
	    while (p > fname) {
		if (*p == ' ') {
		    p++;
		    break;
		}
		p--;
	    }
	    (void) strlcpy(curfile, p, sizeof curfile);
	}
    }
#endif

#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
    if (Crypt) {
	int ret = 0;
	if (*save == '-' && strlen(fname) == 1)
	    error("Can't use encryption in a pipeline.");
	else
	    if (*save == '|')
		error("Can't use encryption with advanced macros.");
	else
	    ret = creadfile(save, eraseflg);
	autolabel = tempautolabel;
	return (ret);
    }
#endif /* VMS */

    if (eraseflg && strcmp(fname, curfile) && modcheck(" first")) return 0;

#ifndef MSDOS
    if (fname[0] == '-' && fname[1] == '\0') {
	f = stdin;
	*save = '\0';
    } else {
#endif /* MSDOS */
	if ((f = openfile(save, sizeof save, &pid, &rfd)) == NULL) {
	    error("Can't read file \"%s\"", save);
	    autolabel = tempautolabel;
	    return 0;
	} else if (eraseflg) {
	    if (usecurses) {
		error("Reading file \"%s\"", save);
		refresh();
	    } else
		fprintf(stderr, "Reading file \"%s\"\n", save);
	}
#ifndef MSDOS
    }
    if (*fname == '|')
	*save = '\0';
#endif /* MSDOS */

    if (eraseflg) erasedb();

    remember(0);
    loading++;
    savefd = macrofd;
    macrofd = rfd;
    while (!brokenpipe && fgets(line, sizeof(line), f)) {
#ifndef MSDOS
	if (line[0] == '|' && pid != 0) {
	    line[0] = ' ';
	}
#endif /* MSDOS */
	linelim = 0;
	if (line[0] != '#') (void) yyparse();
    }
    macrofd = savefd;
    --loading;
    remember(1);
    closefile(f, pid, rfd);
#ifndef MSDOS
    if (f == stdin) {
	freopen("/dev/tty", "r", stdin);
	goraw();
    }
#endif /* MSDOS */
    linelim = -1;
    if (eraseflg) {
	(void) strlcpy(curfile, save, sizeof curfile);
	modflg = 0;
	cellassign = 0;
	if (autorun && !skipautorun) (void) readfile(autorun, 0);
	skipautorun = 0;
	EvalAll();
    }
    autolabel = tempautolabel;
    FullUpdate++;
    return 1;
}

/* erase the database (tbl, etc.) */
void
erasedb(void) {
    int  r, c;
    char *home;

    for (c = 0; c<=maxcol; c++) {
	fwidth[c] = DEFWIDTH;
	precision[c] = DEFPREC;
	realfmt[c] = DEFREFMT;
    }

    for (r = 0; r<=maxrow; r++) {
	register struct ent **pp = ATBL(tbl, r, 0);
	for (c=0; c++<=maxcol; pp++)
	    if (*pp) {
		if ((*pp)->expr)  efree((*pp) -> expr);
		if ((*pp)->label) scxfree((char *)((*pp) -> label));
		(*pp)->next = freeents;	/* save [struct ent] for reuse */
		freeents = *pp;
		*pp = (struct ent *)0;
	    }
    }

    for (c = 0; c < COLFORMATS; c++)
	if (colformat[c]) {
	    scxfree(colformat[c]);
	    colformat[c] = NULL;
	}

    maxrow = 0;
    maxcol = 0;
    clean_range();
    clean_frange();
    clean_crange();

    propagation = 10;
    calc_order = BYROWS;
    prescale = 1.0;
    tbl_style = 0;
    craction = 0;
    rowlimit = collimit = -1;
    qbuf = 0;

    autocalc=showcell=showtop=1;
    autoinsert=autowrap=optimize=numeric=extfunc=color=colorneg=colorerr=cslop=0;
    currow=curcol=strow=stcol=0;
    if (usecurses && has_colors())
	color_set(0, NULL);
    /* unset all marks */
    for (c = 1; c < 37; c++)
	savedrow[c] = savedcol[c] = savedstrow[c] = savedstcol[c] = -1;

    if (mdir) {
	scxfree(mdir);
	mdir = NULL;
    }
    if (autorun) {
	scxfree(autorun);
	autorun = NULL;
    }
    for (c = 0; c < FKEYS; c++)
	if (fkey[c]) {
	    scxfree(fkey[c]);
	    fkey[c] = NULL;
	}

#ifndef MSDOS
    /*
     * Load $HOME/.scrc if present.
     */
    if ((home = getenv("HOME"))) {
	strlcpy(curfile, home, sizeof curfile);
	strlcat(curfile, "/.scrc", sizeof curfile);
	if ((c = open(curfile, O_RDONLY)) > -1) {
	    close(c);
	    (void) readfile(curfile, 0);
	}
    }

    /*
     * Load ./.scrc if present and $HOME/.scrc contained `set scrc'.
     */
    if (scrc && strcmp(home, getcwd(curfile, PATHLEN)) &&
	    (c = open(".scrc", O_RDONLY)) > -1) {
	close(c);
	(void) readfile(".scrc", 0);
    }
#endif /* MSDOS */

    *curfile = '\0';
    FullUpdate++;
}

/* moves curcol back one displayed column */
void
backcol(int arg)
{
    while (--arg >= 0) {
	if (curcol)
	    curcol--;
	else
	    {error ("At column A"); break;}
	while(col_hidden[curcol] && curcol)
	    curcol--;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves curcol forward one displayed column */
void
forwcol(int arg)
{
    while (--arg>=0) {
	if (curcol < maxcols - 1)
	    curcol++;
	else
	if (!growtbl(GROWCOL, 0, arg))	/* get as much as needed */
		break;
	else
		curcol++;
	while (col_hidden[curcol] && (curcol < maxcols - 1))
	    curcol++;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves currow forward one displayed row */
void
forwrow(int arg)
{
    while (--arg>=0) {
	if (currow < maxrows - 1)
	    currow++;
	else
	if (!growtbl(GROWROW, arg, 0))	/* get as much as needed */
		break;
	else
		currow++;
	while (row_hidden[currow]&&(currow<maxrows-1))
	    currow++;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves currow backward one displayed row */
void
backrow(int arg)
{
    while (--arg>=0) {
	if (currow)
	    currow--;
	else
	    {error("At row zero"); break;}
	while (row_hidden[currow] && currow)
	    currow--;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

void
markcell(void)
{
    int c;

    error("Mark cell:");
    if ((c=nmgetch()) == ESC || c == ctl('g')) {
	CLEAR_LINE;
	return;
    }
    if ((c -= ('a' - 1)) < 1 || c > 26) {
	error("Invalid mark (must be a-z)");
	return;
    }
    CLEAR_LINE;
    savedrow[c] = currow;
    savedcol[c] = curcol;
    savedstrow[c] = strow;
    savedstcol[c] = stcol;
}

void
dotick(int tick)
{
    int c;

    remember(0);

    error("Go to marked cell:");
    if ((c = nmgetch()) == ESC || c == ctl('g')) {
	CLEAR_LINE;
	return;
    }
    if (c == '`' || c == '\'')
	c = 0;
    else if (!(((c -= ('a' - 1)) > 0 && c < 27) ||
	    ((c += ('a' - '0' + 26)) > 26 && c < 37))) {
	error("Invalid mark (must be a-z, 0-9, ` or \')");
	return;
    }
    if (savedrow[c] == -1) {
	error("Mark not set");
	return;
    }
    CLEAR_LINE;
    currow = savedrow[c];
    curcol = savedcol[c];
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    if (tick == '\'') {
	strow = savedstrow[c];
	stcol = savedstcol[c];
	gs.stflag = 1;
    } else
	gs.stflag = 0;
    remember(1);

    FullUpdate++;
}

void
gotonote(void)
{
    register struct ent *p;

    p = lookat(currow, curcol);
    if (p->nrow == -1) {
	error("No note attached");
    } else
	moveto(p->nrow, p->ncol, p->nlastrow, p->nlastcol, -1, -1);
}

/*
 * Show a cell's label string or expression value.  May overwrite value if
 * there is one already displayed in the cell.  Created from old code in
 * update(), copied with minimal changes.
 */

void
showstring(char *string,	/* to display */
    int dirflush,		/* or rightflush or centered */
    int hasvalue,		/* is there a numeric value? */
    int row, int col,		/* spreadsheet location */
    int *nextcolp,		/* value returned through it */
    int mxcol,			/* last column displayed? */
    int *fieldlenp,		/* value returned through it */
    int r, int c,		/* screen row and column */
    struct frange *fr,		/* frame range we're currently in, if any */
    int frightcols,		/* number of frame columns to the right */
    int flcols, int frcols)	/* width of left and right sides of frame */
{
    int nextcol  = *nextcolp;
    int fieldlen = *fieldlenp;
    char field[FBUFLEN];
    int  slen;
    char *start, *last;
    char *fp, *sp;
    struct ent *nc;
    struct crange *cr;

    cr = find_crange(row, col);

    /* This figures out if the label is allowed to
       slop over into the next blank field */

    slen = strlen(string);
    for (sp = string; *sp != '\0'; sp++)
	if (*sp == '\\' && *(sp + 1) == '"')
	    slen--;
    if (*string == '\\' && *(string+1) != '\0' && *(string+1) != '"')
	slen = fwidth[col];
    if (c + fieldlen == rescol + flcols && nextcol < stcol)
	nextcol = stcol;
    if (frightcols &&
	    c + fieldlen + fwidth[nextcol] >= COLS - 1 - frcols &&
	    nextcol < fr->or_right->col - frightcols + 1)
	nextcol = fr->or_right->col - frightcols + 1;
    while ((slen > fieldlen) && (nextcol <= mxcol) &&
	    !((nc = lookat(row, nextcol))->flags & IS_VALID) && !(nc->label) &&
	    (cslop || find_crange(row, nextcol) == cr)) {

	if (!col_hidden[nextcol])
	    fieldlen += fwidth[nextcol];
	nextcol++;
	if (c + fieldlen == rescol + flcols && nextcol < stcol)
	    nextcol = stcol;
	if (frightcols &&
		c + fieldlen + fwidth[nextcol] >= COLS - 1 - frcols &&
		nextcol < fr->or_right->col - frightcols + 1)
	    nextcol = fr->or_right->col - frightcols + 1;
    }
    if (slen > fieldlen)
	slen = fieldlen;

    /* Now justify and print */
    start = (dirflush&IS_LEFTFLUSH) ? field : field + fieldlen - slen;
    if (dirflush & IS_LABEL)
	start = field + ((slen<fwidth[col])?(fieldlen-slen)/2:0);
    last = field+fieldlen;
    fp = field;
    if (slen)
	while (fp < start)
	    *fp++ = ' ';
    if (*string == '\\' && *(string+1) != '\0' && *(string+1) != '"') {
	char *strt;
	strt = ++string;

	while (slen--) {
	    if (*string == '\\' && *(string + 1) == '"') {
		slen++;
		string++;
	    } else
		*fp++ = *string++;
	    if (*string == '\0')
		string = strt;
	}
    } else
	while (slen--) {
	    if (*string == '\\' && *(string + 1) == '"') {
		slen++;
		string++;
	    } else
		*fp++ = *string++;
	}

    if ((!hasvalue) || fieldlen != fwidth[col]) 
	while (fp < last)
	    *fp++ = ' ';
    *fp = '\0';
    for (fp = field; *fp != '\0'; fp++)
	if (*fp == '\\' && *(fp + 1) == '"')
	    memmove(fp, fp + 1, strlen(fp));
#ifdef VMS
    mvaddstr(r, c, field);	/* this is a macro */
#else
    (void) mvaddstr(r, c, field);
#endif

    *nextcolp  = nextcol;
    *fieldlenp = fieldlen;
}

int
etype(register struct enode *e)
{
    if (e == (struct enode *)0)
	return NUM;
    switch (e->op) {
	case UPPER: case LOWER: case CAPITAL:
	case O_SCONST: case '#': case DATE: case FMT: case STINDEX:
	case EXT: case SVAL: case SUBSTR:
	    return (STR);

	case '?':
	case IF:
	    return (etype(e->e.o.right->e.o.left));

	case 'f':
	    return (etype(e->e.o.right));

	case O_VAR: {
	    register struct ent *p;
	    p = e->e.v.vp;
	    if (p->expr) 
		return (p->flags & IS_STREXPR ? STR : NUM);
	    else if (p->label)
		return (STR);
	    else
		return (NUM);
	    }

	default:
	    return (NUM);
    }
}

/* return 1 if yes given, 0 otherwise */
int
yn_ask(char *msg)
{	char ch;

	(void) move(0, 0);
	(void) clrtoeol();
	(void) addstr(msg);
	(void) refresh();
	while ((ch = nmgetch()) != 'y' && ch != 'Y' && ch != 'n' && ch != 'N') {
	    if (ch == ctl('g') || ch == ESC)
		return (-1);
	}
	if (ch == 'y' || ch == 'Y')
	    return (1);
	else
	    return (0);
}

/* expand a ~ in a path to your home directory */
#if !defined(MSDOS) && !defined(VMS)
#include <pwd.h>
#endif
char *
findhome(char *path, size_t pathsiz)
{
    static	char	*HomeDir = NULL;

    if (*path == '~') {
    	char	*pathptr;
	char	tmppath[PATHLEN];

	if (HomeDir == NULL) {
	    HomeDir = getenv("HOME");
	    if (HomeDir == NULL)
		HomeDir = "/";
	}
	pathptr = path + 1;
	if ((*pathptr == '/') || (*pathptr == '\0'))
	    strlcpy(tmppath, HomeDir, sizeof tmppath);
#if !defined(MSDOS) && !defined(VMS)
	else {
	    struct	passwd *pwent;
	    char	*namep;
	    char	name[50];

	    namep = name;
	    while ((*pathptr != '\0') && (*pathptr != '/'))
		    *(namep++) = *(pathptr++);
	    *namep = '\0';
	    if ((pwent = getpwnam(name)) == NULL) {
	    	error("Can't find user %s", name);
		return (NULL);
	    }
	    strlcpy(tmppath, pwent->pw_dir, sizeof tmppath);
	}
#endif
	strlcat(tmppath, pathptr, sizeof tmppath);
	strlcpy(path, tmppath, pathsiz);
    }
    return (path);
}

/*
 * make a backup copy of a file, use the same mode and name in the format
 * [path/]file~
 * return 1 if we were successful, 0 otherwise
 */
int
backup_file(char *path)
{
    struct	stat	statbuf;
    struct	utimbuf	timebuf;
    char	fname[PATHLEN];
    char	tpath[PATHLEN];
#ifdef sequent
    static	char	*buf = NULL;
    static	unsigned buflen = 0;
#else
    char	buf[BUFSIZ];
#endif
    char	*tpp;
    int		infd, outfd;
    int		count;
    mode_t	oldumask;

    /* tpath will be the [path/]file ---> [path/]file~ */
    strlcpy(tpath, path, sizeof tpath);
    if ((tpp = strrchr(tpath, '/')) == NULL)
	tpp = tpath;
    else
	tpp++;
    strlcpy(fname, tpp, sizeof fname);
    snprintf(tpp, sizeof(tpath) - (tpp - tpath), "%s~", fname);

    if (stat(path, &statbuf) == 0) {
#ifdef sequent
	/* if we know the optimum block size, use it */
	if ((statbuf.st_blksize > buflen) || (buf == NULL)) {
	    buflen = statbuf.st_blksize;
	    if ((buf = scxrealloc(buf, buflen)) == (char *)0) {
		buflen = 0;
		return (0);
	    }
	}
#endif

	if ((infd = open(path, O_RDONLY, 0)) < 0)
		return (0);

	oldumask = umask(0);
	outfd = open(tpath, O_TRUNC|O_WRONLY|O_CREAT, statbuf.st_mode);
	umask(oldumask);
	if (outfd < 0)
	    return (0);
	chown(tpath, statbuf.st_uid, statbuf.st_gid);

#ifdef sequent
	while ((count = read(infd, buf, statbuf.st_blksize)) > 0) {
#else
	while ((count = read(infd, buf, sizeof(buf))) > 0) {
#endif
	    if (write(outfd, buf, count) != count) {
		count = -1;
		break;
	    }
	}
	close(infd);
	close(outfd);

	/* copy access and modification times from original file */
	timebuf.actime = statbuf.st_atime;
	timebuf.modtime = statbuf.st_mtime;
	utime(tpath, &timebuf);

	return ((count < 0) ? 0 : 1);
    } else if (errno == ENOENT)
	return (1);
    return (0);
}
