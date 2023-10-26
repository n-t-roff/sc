/*	SC	A Spreadsheet Calculator
 *		Expression interpreter and assorted support routines.
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel, 
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 7.16 $
 */

#include <sys/types.h>
#ifdef aiws
#undef _C_func			/* Fixes for undefined symbols on AIX */
#endif

#ifdef USE_IEEEFP_H
# include <ieeefp.h>
#endif

#include <math.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#ifndef MSDOS
#include <unistd.h>
#endif

#ifdef REGCOMP
#include <regex.h>
#endif
#include "compat.h"
#include "sc.h"
#ifdef RE_COMP
extern char *re_comp(char *s);
extern char *re_exec(char *s);
#endif
#ifdef REGCMP
char *regcmp();
char *regex();
#endif

/* Use this structure to save the last 'g' command */
struct go_save gs;

/* g_type can be: */
#define G_NONE 0	/* Starting value - must be 0 */
#define G_NUM 1
#define G_STR 2
#define G_NSTR 3
#define G_XSTR 4
#define G_CELL 5

#define ISVALID(r,c)	((r)>=0 && (r)<maxrows && (c)>=0 && (c)<maxcols)

jmp_buf fpe_save;
int	exprerr;	/* Set by eval() and seval() if expression errors */
double  prescale = 1.0;	/* Prescale for constants in let() */
int	extfunc  = 0;	/* Enable/disable external functions */
int     loading = 0;	/* Set when readfile() is active */
int	gmyrow, gmycol;	/* globals used to implement @myrow, @mycol cmds */
int	rowoffset = 0, coloffset = 0;	/* row & col offsets for range
					   functions */

extern bool decimal;	/* Set if there was a decimal point in the number */

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
struct enode *freeenodes = NULL;

double	dolookup(struct enode * val, int minr, int minc, int maxr,
	int maxc, int offr, int offc);
double	fn1_eval(double (*fn)(), double arg);
double	fn2_eval(double (*fn)(), double arg1, double arg2);
static int RealEvalAll(void);
int	constant(register struct enode *e);
void	RealEvalOne(register struct ent *p, int i, int j, int *chgct);
void	copydbuf(int deltar, int deltac);
void	decompile(register struct enode *e, int	priority);
void	index_arg(char *s, struct enode *e);
void	list_arg(char *s, struct enode *e);
void	one_arg(char *s, struct enode *e);
void	range_arg(char *s, struct enode *e);
void	three_arg(char *s, struct enode *e);
void	two_arg(char *s, struct enode *e);
void	two_arg_index(char *s, struct enode *e);
static double finfunc(int, double, double, double);
static char *dostindex(int, int, int, int, struct enode *);
static double doindex(int, int, int, int, struct enode *);
static double docount(int, int, int, int, struct enode *);
static double dosum(int, int, int, int, struct enode *);
static double doprod(int, int, int, int, struct enode *);
static double doavg(int, int, int, int, struct enode *);
static double dostddev(int, int, int, int, struct enode *);
static double domax(int, int, int, int, struct enode *);
static double domin(int, int, int, int, struct enode *);
static double dodts(int, int, int);
static double dotts(int, int, int);
static double dotime(int, double);
static double doston(char *);
static double doeqs(char *, char *);
static struct ent *getent(char *, double);
static double donval(char *, double);
static double dolmax(struct enode *);
static double dolmin(struct enode *);
static char *docat(register char *, register char *);
static char *dodate(time_t, char *);
static char *dofmt(char *, double);
static char *doext(struct enode *);
static char *dosval(char *, double);
static void decodev(struct ent_ptr);
static char *docapital(char *);
static char *docase(int, char *);
static char *dosubstr(char *, register int, register int);

double	rint(double d);
int	cellerror = CELLOK;	/* is there an error in this cell */
static void g_free(void);
static
#ifdef SIGVOID
void
#else
int
#endif
eval_fpe(int);

#ifndef M_PI
#define M_PI (double)3.14159265358979323846
#endif
#define dtr(x) ((x)*(M_PI/(double)180.0))
#define rtd(x) ((x)*(180.0/(double)M_PI))

static double
finfunc(int fun, double v1, double v2, double v3)
{
 	double answer,p;
 
 	p = fn2_eval(pow, 1 + v2, v3);
 
 	switch (fun) {
 	    case PV:
		if (v2)
		    answer = v1 * (1 - 1/p) / v2;
		else {
		    cellerror = CELLERROR;
		    answer = (double)0;
		}
 		break;
 	    case FV:
		if (v2)
		    answer = v1 * (p - 1) / v2;
		else {
		    cellerror = CELLERROR;
		    answer = (double)0;
		}
 		break;
 	    case PMT:
		/* CHECK IF ~= 1 - 1/1 */
		if (p && p != (double)1)
		    answer = v1 * v2 / (1 - 1/p);
		else {
		    cellerror = CELLERROR;
		    answer = (double)0;
		}
		
 		break;
	    default:
		error("Unknown function in finfunc");
		cellerror = CELLERROR;
		return ((double)0);
	}
	return (answer);
}

static char *
dostindex(int minr, int minc, int maxr, int maxc, struct enode *val)
{
    int r, c;
    register struct ent *p;
    char *pr;

    p = (struct ent *)0;
    if (minr == maxr) {			/* look along the row */
	r = minr;
	c = minc + (int) eval(val) - 1;
    } else if (minc == maxc) {		/* look down the column */
	r = minr + (int) eval(val) - 1;
	c = minc;
    } else {
	r = minr + (int) eval(val->e.o.left) - 1;
	c = minc + (int) eval(val->e.o.right) - 1;
    }
    if (c <= maxc && c >=minc && r <= maxr && r >=minr)
	p = *ATBL(tbl, r, c);

    if (p && p->label) {
	size_t l = strlen(p->label) + 1;
	pr = scxmalloc(l);
	strlcpy(pr, p->label, l);
	if (p->cellerror)
	    cellerror = CELLINVALID;
	return (pr);
    } else
	return ((char *)0);
}

static double
doindex(int minr, int minc, int maxr, int maxc, struct enode *val)
{
    int r, c;
    register struct ent *p;

    if (val->op == ',') {		/* index by both row and column */
	r = minr + (int) eval(val->e.o.left) - 1;
	c = minc + (int) eval(val->e.o.right) - 1;
    } else if (minr == maxr) {		/* look along the row */
	r = minr;
	c = minc + (int) eval(val) - 1;
    } else if (minc == maxc) {		/* look down the column */
	r = minr + (int) eval(val) - 1;
	c = minc;
    } else {
	error("Improper indexing operation");
	return (double) 0;
    }

    if (c <= maxc && c >=minc && r <= maxr && r >=minr &&
	    (p = *ATBL(tbl, r, c)) && p->flags&IS_VALID) {
	if (p->cellerror)
	    cellerror = CELLINVALID;
	return p->v;
    } else
	return (double) 0;
}

double
dolookup(struct enode * val, int minr, int minc, int maxr, int maxc, int offset, int vflag)
{
    double v, ret = (double)0;
    int r, c;
    register struct ent *p = (struct ent *)0;
    int incr, incc, fndr, fndc;
    char *s;

    incr = vflag; incc = 1 - vflag;
    if (etype(val) == NUM) {
	cellerror = CELLOK;
	v = eval(val);
	for (r = minr, c = minc; r <= maxr && c <= maxc; r+=incr, c+=incc) {
	    if ((p = *ATBL(tbl, r, c)) && p->flags&IS_VALID) {
		if (p->v <= v) {
		    fndr = incc ? (minr + offset) : r;
		    fndc = incr ? (minc + offset) : c;
		    if (ISVALID(fndr, fndc))
			p = *ATBL(tbl, fndr, fndc);
		    else {
			error(" range specified to @[hv]lookup");
			cellerror = CELLERROR;
		    }
		    if (p && p->flags&IS_VALID) {
		    	if (p->cellerror)
			    cellerror = CELLINVALID;
			ret = p->v;
		    }
		} else break;
	    }
	}
    } else {
	cellerror = CELLOK;
	s = seval(val);
	for (r = minr, c = minc; r <= maxr && c <= maxc; r+=incr, c+=incc) {
	    if ((p = *ATBL(tbl, r, c)) && p->label) {
		if (strcmp(p->label,s) == 0) {
		    fndr = incc ? (minr + offset) : r;
		    fndc = incr ? (minc + offset) : c;
		    if (ISVALID(fndr,fndc)) {
		    	p = *ATBL(tbl, fndr, fndc);
			if (p->cellerror)
				cellerror = CELLINVALID;
		    } else {
			error(" range specified to @[hv]lookup");
			cellerror = CELLERROR;
		    }
		    break;
		}
	    }
	}
	if (p && p->flags&IS_VALID)
	    ret = p->v;
	scxfree(s);
    }
    return ret;
}

static double
docount(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    int v;
    int r, c;
    int cellerr = CELLOK;
    register struct ent *p;

    v = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags & IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;
		    v++;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double
dosum(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    double v;
    int r, c;
    int cellerr = CELLOK;
    register struct ent *p;

    v = (double)0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags & IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;
		    v += p->v;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double
doprod(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    double v;
    int r, c;
    int cellerr = CELLOK;
    register struct ent *p;

    v = 1;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags & IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;
		    v *= p->v;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double
doavg(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    double v;
    int r, c;
    int count;
    int cellerr = CELLOK;
    register struct ent *p;

    v = (double)0;
    count = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags&IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;

		    v += p->v;
		    count++;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;

    if (count == 0) 
	return ((double)0);

    return (v / (double)count);
}

static double
dostddev(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    double lp, rp, v, nd;
    int r, c;
    int n;
    int cellerr = CELLOK;
    register struct ent *p;

    n = 0;
    lp = 0;
    rp = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags&IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;

		    v = p->v;
		    lp += v*v;
		    rp += v;
		    n++;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;

    if ((n == 0) || (n == 1)) 
	return ((double)0);
    nd = (double)n;
    return (sqrt((nd*lp-rp*rp)/(nd*(nd-1))));
}

static double
domax(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    double v = (double)0;
    int r, c;
    int count;
    int cellerr = CELLOK;
    register struct ent *p;

    count = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags&IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;

		    if (!count) {
			v = p->v;
			count++;
		    } else if (p->v > v)
			v = p->v;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;

    if (count == 0) 
	return ((double)0);

    return (v);
}

static double
domin(int minr, int minc, int maxr, int maxc, struct enode *e)
{
    double v = (double)0;
    int r, c;
    int count;
    int cellerr = CELLOK;
    register struct ent *p;

    count = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    if (e) {
		rowoffset = r - minr;
		coloffset = c - minc;
	    }
	    if (!e || eval(e))
		if ((p = *ATBL(tbl, r, c)) && p->flags&IS_VALID) {
		    if (p->cellerror)
			cellerr = CELLINVALID;

		    if (!count) {
			v = p->v;
			count++;
		    } else if (p->v < v)
			v = p->v;
		}
	}
    cellerror = cellerr;
    rowoffset = coloffset = 0;

    if (count == 0) 
	return ((double)0);

    return (v);
}

int mdays[12]={ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

static double
dodts(int e1, int e2, int e3)
{
    int		yr, mo, day;
    time_t	secs;
    struct tm	t; 

    if (e2 > 12 || e3 > 31) {
	mo  = e1;
	day = e2;
	yr  = e3;
    } else {
	yr  = e1;
	mo  = e2;
	day = e3;
    }
    mdays[1] = 28 + (yr%4 == 0) - (yr%100 == 0) + (yr%400 == 0);

    t.tm_hour = t.tm_min = t.tm_sec = 0;
    t.tm_mon = --mo;
    t.tm_mday = day;
    t.tm_year = yr -= 1900;
    t.tm_isdst = -1;

    if (mo < 0 || mo > 11 || day < 1 || day > mdays[mo] ||
               (secs = mktime(&t)) == -1) {
        error("@dts: invalid argument or date out of range");
        cellerror = CELLERROR;
        return (0.0);
    }

    return ((double)secs);
}

static double
dotts(int hr, int min, int sec)
{
    if (hr < 0 || hr > 23 || min < 0 || min > 59 || sec < 0 || sec > 59) {
	error ("@tts: Invalid argument");
	cellerror = CELLERROR;
	return ((double) 0);
    }
    return ((double)(sec+min*60+hr*3600));
}

static double
dotime(int which, double when)
{
	static time_t t_cache;
	static struct tm tm_cache;
	struct tm *tp;
	time_t tloc;

	if (which == NOW) 
	    return (double)time(NULL);

	tloc = (time_t)when;

	if (tloc != t_cache) {
	    tp = localtime(&tloc);
	    tm_cache = *tp;
	    tm_cache.tm_mon += 1;
	    tm_cache.tm_year += 1900;
	    t_cache = tloc;
	}

	switch (which) {
	    case HOUR:		return ((double)(tm_cache.tm_hour));
	    case MINUTE:	return ((double)(tm_cache.tm_min));
	    case SECOND:	return ((double)(tm_cache.tm_sec));
	    case MONTH:		return ((double)(tm_cache.tm_mon));
	    case DAY:		return ((double)(tm_cache.tm_mday));
	    case YEAR:		return ((double)(tm_cache.tm_year));
	}
	/* Safety net */
	cellerror = CELLERROR;
	return ((double)0);
}

static double
doston(char *s)
{
    double v;

    if (!s)
	return ((double)0);

    v = strtod(s, NULL);
    scxfree(s);
    return(v);
}

static double
doeqs(char *s1, char *s2)
{
    double v;

    if (!s1 && !s2)
	return ((double)1.0);

    if (!s1 || !s2)
	v = 0.0;
    else if (strcmp(s1, s2) == 0)
	v = 1.0;
    else
	v = 0.0;

    if (s1)
    	scxfree(s1);

    if (s2)
    	scxfree(s2);

    return(v);
}


/*
 * Given a string representing a column name and a value which is a row
 * number, return a pointer to the selected cell's entry, if any, else NULL.
 * Use only the integer part of the column number.  Always free the string.
 */

static struct ent *
getent(char *colstr, double rowdoub)
{
    int collen;		/* length of string */
    int row, col;	/* integer values   */
    struct ent *p = (struct ent *)0;	/* selected entry   */

    if (!colstr) {
    	cellerror = CELLERROR;
	return ((struct ent *)0);
    }

    if (((row = (int) floor(rowdoub)) >= 0)
	    && (row < maxrows)				/* in range */
	    && ((collen = strlen (colstr)) <= 2)	/* not too long */
	    && ((col = atocol (colstr, collen)) >= 0)
	    && (col < maxcols)) {			/* in range */
	p = *ATBL(tbl, row, col);
	if ((p != NULL) && p->cellerror)
	    cellerror = CELLINVALID;
    }
    scxfree(colstr);
    return (p);
}


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

static double
donval(char *colstr, double rowdoub)
{
    struct ent *ep;

    return (((ep = getent(colstr, rowdoub)) && ((ep->flags) & IS_VALID)) ?
	    (ep->v) : (double)0);
}


/*
 *	The list routines (e.g. dolmax) are called with an LMAX enode.
 *	The left pointer is a chain of ELIST nodes, the right pointer
 *	is a value.
 */
static double
dolmax(struct enode *ep)
{
    register int count = 0;
    register double maxval = 0; /* Assignment to shut up lint */
    register struct enode *p;
    register double v;

    cellerror = CELLOK;
    for (p = ep; p; p = p->e.o.left) {
	v = eval(p->e.o.right);
	if (!count || v > maxval)
	    maxval = v, count++;
    }
    if (count) return maxval;
    else return (double)0;
}

static double
dolmin(struct enode *ep)
{
    register int count = 0;
    register double minval = 0; /* Assignment to shut up lint */
    register struct enode *p;
    register double v;

    cellerror = CELLOK;
    for (p = ep; p; p = p->e.o.left) {
	v = eval(p->e.o.right);
	if (!count || v < minval)
	    minval = v, count++;
    }
    if (count) return minval;
    else return (double)0;
}

double 
eval(register struct enode *e)
{
    if (e == (struct enode *)0) {
    	cellerror = CELLINVALID;
	return (double)0;
    }
    switch (e->op) {
	case '+':	return (eval(e->e.o.left) + eval(e->e.o.right));
	case '-':	{
			double l, r;
			l = eval(e->e.o.left);
			r = eval(e->e.o.right);
			return l - r;
			}
	case '*':	return (eval(e->e.o.left) * eval(e->e.o.right));
	case '/':	{
			double num, denom;
			num = eval(e->e.o.left);
			denom = eval(e->e.o.right);
			if (cellerror) {
			    cellerror = CELLINVALID;
			    return ((double) 0);
			} else
			if (denom)
			    return (num/denom);
			else {
			    cellerror = CELLERROR;
			    return ((double) 0);
			}
	}
	case '%':	{
			double num, denom;
			num = floor(eval(e->e.o.left));
			denom = floor(eval(e->e.o.right));
			if (denom)
			    return (num - floor(num/denom)*denom);
			else {
			    cellerror = CELLERROR;
			    return ((double) 0);
			}
	}
	case '^':	return (fn2_eval(pow,eval(e->e.o.left),eval(e->e.o.right)));
	case '<':	return (eval(e->e.o.left) < eval(e->e.o.right));
	case '=':	{
			double l, r;
			l = eval(e->e.o.left);
			r = eval(e->e.o.right);
			return (l == r);
			}
	case '>':	return (eval(e->e.o.left) > eval(e->e.o.right));
	case '&':	return (eval(e->e.o.left) && eval(e->e.o.right));
	case '|':	return (eval(e->e.o.left) || eval(e->e.o.right));
	case IF:
	case '?':	return eval(e->e.o.left) ? eval(e->e.o.right->e.o.left)
						: eval(e->e.o.right->e.o.right);
	case 'm':	return (-eval(e->e.o.left));
	case 'f':	{
			int rtmp = rowoffset;
			int ctmp = coloffset;
			double ret;
			rowoffset = coloffset = 0;
			ret = eval(e->e.o.left);
			rowoffset = rtmp;
			coloffset = ctmp;
			return (ret);
			}
	case 'F':	return (eval(e->e.o.left));
	case '!':	return (eval(e->e.o.left) == 0.0);
	case ';':	return (((int)eval(e->e.o.left) & 7) +
				(((int)eval(e->e.o.right) & 7) << 3));
	case O_CONST:	if (!
#ifdef HAVE_ISFINITE
			  isfinite(
#else
			  finite(
#endif
			  e->e.k)) {
			    e->op = ERR_;
			    e->e.k = (double) 0;
			    cellerror = CELLERROR;
			}
			return (e->e.k);
	case O_VAR:	{
			struct ent *vp = e->e.v.vp;
			int row, col;
			if (vp && (rowoffset || coloffset)) {
			    row = e->e.v.vf & FIX_ROW ? vp->row
						      : vp->row + rowoffset;
			    col = e->e.v.vf & FIX_COL ? vp->col
						      : vp->col + coloffset;
			    checkbounds(&row, &col);
			    vp = *ATBL(tbl, row, col);
			}
			if (!vp || vp->flags & IS_DELETED) {
			    cellerror = CELLERROR;
			    return (double) 0;
			}
			if (vp->cellerror)
			    cellerror = CELLINVALID;
			return (vp->v);
			}
	case SUM:
 	case PROD:
 	case AVG:
 	case COUNT:
 	case STDDEV:
	case MAX:
	case MIN:
	case INDEX:
	case LOOKUP:
	case HLOOKUP:
	case VLOOKUP: {
	    int r, c;
	    int maxr, maxc;
	    int minr, minc;
	    maxr = e->e.o.left->e.r.right.vp->row;
	    maxc = e->e.o.left->e.r.right.vp->col;
	    minr = e->e.o.left->e.r.left.vp->row;
	    minc = e->e.o.left->e.r.left.vp->col;
	    if (minr>maxr) r = maxr, maxr = minr, minr = r;
	    if (minc>maxc) c = maxc, maxc = minc, minc = c;
		switch (e->op) {
		    case LOOKUP:
			return dolookup(e->e.o.right, minr, minc, maxr, maxc,
				1, minc==maxc);
		    case HLOOKUP:
			return dolookup(e->e.o.right->e.o.left, minr,minc,maxr,maxc,
			    (int) eval(e->e.o.right->e.o.right), 0);
		    case VLOOKUP:
			return dolookup(e->e.o.right->e.o.left, minr,minc,maxr,maxc,
			    (int) eval(e->e.o.right->e.o.right), 1);
		    case INDEX:
			return doindex(minr, minc, maxr, maxc, e->e.o.right);
	            case SUM:
			return dosum(minr, minc, maxr, maxc, e->e.o.right);
	            case PROD:
			return doprod(minr, minc, maxr, maxc, e->e.o.right);
	            case AVG:
			return doavg(minr, minc, maxr, maxc, e->e.o.right);
	            case COUNT:
			return docount(minr, minc, maxr, maxc, e->e.o.right);
	            case STDDEV:
			return dostddev(minr, minc, maxr, maxc, e->e.o.right);
	            case MAX:
			return domax(minr, minc, maxr, maxc, e->e.o.right);
	            case MIN:
			return domin(minr, minc, maxr, maxc, e->e.o.right);
		}
	}
 	case REDUCE | 'R':
 	case REDUCE | 'C':
	    {	int r, c;
		int maxr, maxc;
		int minr, minc;
		maxr = e->e.r.right.vp->row;
		maxc = e->e.r.right.vp->col;
		minr = e->e.r.left.vp->row;
		minc = e->e.r.left.vp->col;
		if (minr>maxr) r = maxr, maxr = minr, minr = r;
		if (minc>maxc) c = maxc, maxc = minc, minc = c;
	        switch (e->op) {
 	            case REDUCE | 'R': return (maxr - minr + 1);
 	            case REDUCE | 'C': return (maxc - minc + 1);
		}
	    }
	case ABS:	 return (fn1_eval( fabs, eval(e->e.o.left)));
	case ACOS:	 return (fn1_eval( acos, eval(e->e.o.left)));
	case ASIN:	 return (fn1_eval( asin, eval(e->e.o.left)));
	case ATAN:	 return (fn1_eval( atan, eval(e->e.o.left)));
	case ATAN2:	 return (fn2_eval( atan2, eval(e->e.o.left), eval(e->e.o.right)));
	case CEIL:	 return (fn1_eval( ceil, eval(e->e.o.left)));
	case COS:	 return (fn1_eval( cos, eval(e->e.o.left)));
	case EXP:	 return (fn1_eval( exp, eval(e->e.o.left)));
	case FABS:	 return (fn1_eval( fabs, eval(e->e.o.left)));
	case FLOOR:	 return (fn1_eval( floor, eval(e->e.o.left)));
	case HYPOT:	 return (fn2_eval( hypot, eval(e->e.o.left), eval(e->e.o.right)));
	case LOG:	 return (fn1_eval( log, eval(e->e.o.left)));
	case LOG10:	 return (fn1_eval( log10, eval(e->e.o.left)));
	case POW:	 return (fn2_eval( pow, eval(e->e.o.left), eval(e->e.o.right)));
	case SIN:	 return (fn1_eval( sin, eval(e->e.o.left)));
	case SQRT:	 return (fn1_eval( sqrt, eval(e->e.o.left)));
	case TAN:	 return (fn1_eval( tan, eval(e->e.o.left)));
	case DTR:	 return (dtr(eval(e->e.o.left)));
	case RTD:	 return (rtd(eval(e->e.o.left)));
	case RND:
	    if (rndtoeven)
		return rint(eval(e->e.o.left));
	    else {
		double temp = eval(e->e.o.left);
		return (temp - floor(temp) < 0.5 ? floor(temp) : ceil(temp));
	    }
 	case ROUND:
	    {	int prec = (int) eval(e->e.o.right);
		double scale = 1;
		if (0 < prec)
		    do scale *= 10; while (0 < --prec);
		else if (prec < 0)
		    do scale /= 10; while (++prec < 0);

		if (rndtoeven)
		    return (rint(eval(e->e.o.left) * scale) / scale);
		else {
		    double temp = eval(e->e.o.left);
		    temp *= scale;
		    /* xxx */
		    /*
		    temp = (temp > 0.0 ? floor(temp + 0.5) : ceil(temp - 0.5));
		    */
		    temp = ((temp - floor(temp)) < 0.5 ?
			    floor(temp) : ceil(temp));
		    return (temp / scale);
		}
	    }
	case FV:
	case PV:
	case PMT:	return (finfunc(e->op, eval(e->e.o.left),
				    eval(e->e.o.right->e.o.left),
				    eval(e->e.o.right->e.o.right)));
	case HOUR:	return (dotime(HOUR, eval(e->e.o.left)));
	case MINUTE:	return (dotime(MINUTE, eval(e->e.o.left)));
	case SECOND:	return (dotime(SECOND, eval(e->e.o.left)));
	case MONTH:	return (dotime(MONTH, eval(e->e.o.left)));
	case DAY:	return (dotime(DAY, eval(e->e.o.left)));
	case YEAR:	return (dotime(YEAR, eval(e->e.o.left)));
	case NOW:	return (dotime(NOW, (double)0.0));
	case DTS:	return (dodts((int)eval(e->e.o.left),
				    (int)eval(e->e.o.right->e.o.left),
				    (int)eval(e->e.o.right->e.o.right)));
	case TTS:	return (dotts((int)eval(e->e.o.left),
				    (int)eval(e->e.o.right->e.o.left),
				    (int)eval(e->e.o.right->e.o.right)));
	case STON:	return (doston(seval(e->e.o.left)));
	case EQS:       return (doeqs(seval(e->e.o.right), seval(e->e.o.left)));
	case LMAX:	return dolmax(e);
	case LMIN:	return dolmin(e);
	case NVAL:      return (donval(seval(e->e.o.left),eval(e->e.o.right)));
	case MYROW:	return ((double) (gmyrow + rowoffset));
	case MYCOL:	return ((double) (gmycol + coloffset));
	case LASTROW:	return ((double) maxrow);
	case LASTCOL:	return ((double) maxcol);
	case NUMITER:	return ((double) repct);
	case ERR_:	cellerror = CELLERROR;
			return ((double) 0);
	case PI_:	return ((double) M_PI);
	case BLACK:	return ((double) COLOR_BLACK);
	case RED:	return ((double) COLOR_RED);
	case GREEN:	return ((double) COLOR_GREEN);
	case YELLOW:	return ((double) COLOR_YELLOW);
	case BLUE:	return ((double) COLOR_BLUE);
	case MAGENTA:	return ((double) COLOR_MAGENTA);
	case CYAN:	return ((double) COLOR_CYAN);
	case WHITE:	return ((double) COLOR_WHITE);
	default:	error ("Illegal numeric expression");
			exprerr = 1;
    }
    cellerror = CELLERROR;
    return ((double)0.0);
}

static
#ifdef SIGVOID
void
#else
int
#endif
eval_fpe(int i) /* Trap for FPE errors in eval */
{
    (void)i;
#if defined(i386) && !defined(M_XENIX)
    asm("	fnclex");
    asm("	fwait");
#else
#ifdef IEEE_MATH
    (void)fpsetsticky((fp_except)0);	/* Clear exception */
#endif /* IEEE_MATH */
#ifdef PC
    _fpreset();
#endif
#endif
    /* re-establish signal handler for next time */
    (void) signal(SIGFPE, eval_fpe);
    longjmp(fpe_save, 1);
}

double
fn1_eval(double (*fn)(), double a)
{
    double res;
    errno = 0;
    res = (*fn)(a);
    if (errno)
	cellerror = CELLERROR;

    return res;
}

double
fn2_eval(double (*fn)(), double arg1, double arg2)
{
    double res;
    errno = 0;
    res = (*fn)(arg1, arg2);
    if (errno)
	cellerror = CELLERROR;

    return res;
}

/* 
 * Rules for string functions:
 * Take string arguments which they scxfree.
 * All returned strings are assumed to be xalloced.
 */

static char *
docat(register char *s1, register char *s2)
{
    register char *p;
    char *arg1, *arg2;
    size_t l;

    if (!s1 && !s2)
	return ((char *)0);
    arg1 = s1 ? s1 : "";
    arg2 = s2 ? s2 : "";
    l = strlen(arg1) + strlen(arg2) + 1;
    p = scxmalloc(l);
    strlcpy(p, arg1, l);
    strlcat(p, arg2, l);
    if (s1)
        scxfree(s1);
    if (s2)
        scxfree(s2);
    return (p);
}

static char *
dodate(time_t tloc, char *fmtstr)
{
    char buff[FBUFLEN];
    char *p;
    size_t l;

    if (!fmtstr)
	fmtstr = "%a %b %d %H:%M:%S %Y";
    strftime(buff, FBUFLEN, fmtstr, localtime(&tloc));
    l = strlen(buff) + 1;
    p = scxmalloc(l);
    strlcpy(p, buff, l);
    return (p);
}


static char *
dofmt(char *fmtstr, double v)
{
    char buff[FBUFLEN];
    char *p;
    size_t l;

    if (!fmtstr)
	return ((char *)0);
    (void) snprintf(buff, FBUFLEN, fmtstr, v);
    l = strlen(buff) + 1;
    p = scxmalloc(l);
    strlcpy(p, buff, l);
    scxfree(fmtstr);
    return (p);
}


/*
 * Given a command name and a value, run the command with the given value and
 * read and return its first output line (only) as an allocated string, always
 * a copy of se->e.o.s, which is set appropriately first unless external
 * functions are disabled, in which case the previous value is used.  The
 * handling of se->e.o.s and freeing of command is tricky.  Returning an
 * allocated string in all cases, even if null, insures cell expressions are
 * written to files, etc.
 */

#if defined(VMS) || defined(MSDOS)
char *
doext(struct enode *se)
{
    char *command = seval(se->e.o.left);
    double value = eval(se->e.o.right);
    char *buf;

    error("Warning: External functions unavailable on VMS");
    cellerror = CELLERROR;	/* not sure if this should be a cellerror */
    if (command)
	scxfree(command);
    buf = scxmalloc(1);
    strlcpy(buf, "\0", 1);
    return buf;
}

#else /* if defined(VMS) || defined(MSDOS) */

static char *
doext(struct enode *se)
{
    char buff[FBUFLEN];		/* command line/return, not permanently alloc */
    char *command;
    double value;
    char *buf;
    size_t l;

    command = seval(se->e.o.left);
    value = eval(se->e.o.right);
    if (!extfunc) {
	error("Warning: external functions disabled; using %s value",
		(se->e.o.s && *se->e.o.s) ? "previous" : "null");

	if (command) scxfree(command);
    } else {
	if ((! command) || (! *command)) {
	    error ("Warning: external function given null command name");
	    cellerror = CELLERROR;
	    if (command) scxfree(command);
	} else {
	    FILE *pp;

	    snprintf(buff, sizeof buff, "%s %g", command, value); /* build cmd line */
	    scxfree(command);

	    error("Running external function...");
	    (void) refresh();

	    if ((pp = popen(buff, "r")) == (FILE *) NULL) {	/* run it */
		error("Warning: running \"%s\" failed", buff);
		cellerror = CELLERROR;
	    } else {
		if (fgets(buff, sizeof(buff)-1, pp) == NULL) {	/* one line */
		    error("Warning: external function returned nothing");
		} else {
		    char *cp;

		    CLEAR_LINE;				/* erase notice */
		    buff[sizeof(buff)-1] = '\0';

		    if ((cp = strchr(buff, '\n')))	/* contains newline */
			*cp = '\0';			/* end string there */

		    l = strlen(buff);
		    if (!se->e.o.s || l != strlen(se->e.o.s))
			se->e.o.s = scxrealloc(se->e.o.s, l);
		    strlcpy(se->e.o.s, buff, l);
			 /* save alloc'd copy */
		}
		(void) pclose(pp);

	    } /* else */
	} /* else */
    } /* else */
    if (se->e.o.s) {
	l = strlen(se->e.o.s) + 1;
	buf = scxmalloc(l);
	strlcpy(buf, se->e.o.s, l);
    } else {
	buf = scxmalloc(1);
	strlcpy(buf, "", 1);
    }
    return buf;
}

#endif  /* if !(defined(VMS) || defined(MSDOS)) */


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's string value, if any.  Even if none,
 * still allocate and return a null string so the cell has a label value so
 * the expression is saved in a file, etc.
 */

static char *
dosval(char *colstr, double rowdoub)
{
    struct ent *ep;
    char *llabel;
    char *buf;
    size_t l;

    llabel = (ep = getent(colstr, rowdoub)) ? (ep -> label) : "";
    if (llabel == 0) llabel = "";
    l = strlen(llabel) + 1;
    buf = scxmalloc(l);
    strlcpy(buf, llabel, l);
    return buf;
}


/*
 * Substring:  Note that v1 and v2 are one-based to users, but zero-based
 * when calling this routine.
 */

static char *
dosubstr(char *s, register int v1, register int v2)
{
    register char *s1, *s2;
    char *p;

    if (!s)
	return ((char *)0);

    if (v2 >= (ssize_t)strlen(s))		/* past end */
	v2 =  strlen(s) - 1;		/* to end   */

    if (v1 < 0 || v1 > v2) {		/* out of range, return null string */
	scxfree(s);
	p = scxmalloc(1);
	p[0] = '\0';
	return (p);
    }
    s2 = p = scxmalloc(v2-v1+2);
    s1 = &s[v1];
    for (; v1 <= v2; s1++, s2++, v1++)
	*s2 = *s1;
    *s2 = '\0';
    scxfree(s);
    return(p);
}

/*
 * character casing: make upper case, make lower case
 */

static char *
docase(int acase, char *s)
{
    char *p = s;

    if (s == NULL)
	return(NULL);

    if( acase == UPPER ) {
        while( *p != '\0' ) {
           if( islower((int)*p) )
		*p = toupper((int)*p);
	   p++;
	}
    }
    else if (acase == LOWER) {
	while (*p != '\0') {
	    if (isupper((int)*p))
		*p = tolower((int)*p);
	    p++;
	}
    }
    return (s);
}

/*
 * make proper capitals of every word in a string
 * if the string has mixed case we say the string is lower
 *	and we will upcase only first letters of words
 * if the string is all upper we will lower rest of words.
 */

static char *
docapital(char *s)
{
    char *p;
    int skip = 1;
    int AllUpper = 1;

    if (s == NULL)
	return (NULL);
    for (p = s; *p != '\0' && AllUpper != 0; p++)
	if (isalpha((int)*p) && islower((int)*p))  AllUpper = 0;
    for (p = s; *p != '\0'; p++) {
	if (!isalnum((int)*p))
		skip = 1;
	else
	if (skip == 1) {
	    skip = 0;
	    if (islower((int)*p))
		*p = toupper((int)*p);
	}
	else	/* if the string was all upper before */
        if (isupper((int)*p) && AllUpper != 0)
	    *p = tolower((int)*p);
    }
    return (s);
}

char *
seval(register struct enode *se)
{
    register char *p;
    size_t l;
    char *buf;

    if (se == (struct enode *)0) return (char *)0;
    switch (se->op) {
	case O_SCONST:
		l = strlen(se->e.s) + 1;
		p = scxmalloc(l);
		strlcpy(p, se->e.s, l);
		return (p);
	case O_VAR:    {
			struct ent *vp = se->e.v.vp;
			int row, col;
			if (vp && (rowoffset || coloffset)) {
			    row = se->e.v.vf & FIX_ROW ? vp->row
						       : vp->row + rowoffset;
			    col = se->e.v.vf & FIX_COL ? vp->col
						       : vp->col + coloffset;
			    checkbounds(&row, &col);
			    vp = *ATBL(tbl, row, col);
			}
			if (!vp || !vp->label)
			    return (NULL);
			l = strlen(vp->label) + 1;
			p = scxmalloc(l);
			strlcpy(p, vp->label, l);
			return (p);
	}
	case '#':    return (docat(seval(se->e.o.left), seval(se->e.o.right)));
	case 'f':    {
		     int rtmp = rowoffset;
		     int ctmp = coloffset;
		     char *ret;
		     rowoffset = coloffset = 0;
		     ret = seval(se->e.o.left);
		     rowoffset = rtmp;
		     coloffset = ctmp;
		     return (ret);
		     }
	case 'F':    return (seval(se->e.o.left));
	case IF:
	case '?':    return (eval(se->e.o.left) ? seval(se->e.o.right->e.o.left)
					     : seval(se->e.o.right->e.o.right));
	case DATE:   return (dodate((time_t)(eval(se->e.o.left)),
				 seval(se->e.o.right)));
	case FMT:    return (dofmt(seval(se->e.o.left), eval(se->e.o.right)));
	case UPPER:  return (docase(UPPER, seval(se->e.o.left)));
	case LOWER:  return (docase(LOWER, seval(se->e.o.left)));
	case CAPITAL:return (docapital(seval(se->e.o.left)));
 	case STINDEX: {
	    int r, c;
	    int maxr, maxc;
	    int minr, minc;
	    maxr = se->e.o.left->e.r.right.vp->row;
	    maxc = se->e.o.left->e.r.right.vp->col;
	    minr = se->e.o.left->e.r.left.vp->row;
	    minc = se->e.o.left->e.r.left.vp->col;
	    if (minr>maxr) r = maxr, maxr = minr, minr = r;
	    if (minc>maxc) c = maxc, maxc = minc, minc = c;
	    return dostindex(minr, minc, maxr, maxc, se->e.o.right);
	}
	case EXT:    return (doext(se));
	case SVAL:   return (dosval(seval(se->e.o.left), eval(se->e.o.right)));
	case SUBSTR: return (dosubstr(seval(se->e.o.left),
			    (int)eval(se->e.o.right->e.o.left) - 1,
			    (int)eval(se->e.o.right->e.o.right) - 1));
	case COLTOA:
	    buf = scxmalloc(10);
	    strlcpy(buf, coltoa((int)eval(se->e.o.left)), 10);
	    return buf;
	case FILENAME: {
		     int n = eval(se->e.o.left);
		     char *s = strrchr(curfile, '/');

		     if (n || s++ == NULL) s = curfile;
		     l = strlen(s) + 1;
		     p = scxmalloc(l);
		     strlcpy(p, s, l);
		     return (p);
	}
	default:
		     error("Illegal string expression");
		     exprerr = 1;
		     return (NULL);
    }
}

/*
 * The graph formed by cell expressions which use other cells's values is not
 * evaluated "bottom up".  The whole table is merely re-evaluated cell by cell,
 * top to bottom, left to right, in RealEvalAll().  Each cell's expression uses
 * constants in other cells.  However, RealEvalAll() notices when a cell gets a
 * new numeric or string value, and reports if this happens for any cell.
 * EvalAll() repeats calling RealEvalAll() until there are no changes or the
 * evaluation count expires.
 */

int propagation = 10;	/* max number of times to try calculation */
int repct = 1;		/* Make repct a global variable so that the 
				function @numiter can access it */

void
setiterations(int i)
{
    if (i < 1) {
	error("iteration count must be at least 1");
	propagation = 1;
    } else
	propagation = i;
}

void
EvalAll(void) {
    int lastcnt, pair, v;
  
    repct = 1;
    (void) signal(SIGFPE, eval_fpe);

    while ((lastcnt = RealEvalAll()) && (++repct <= propagation));
    if ((propagation > 1) && (lastcnt > 0))
 	error("Still changing after %d iterations", repct - 1);

    if (usecurses && color && has_colors()) {
	for (pair = 0; pair < 8; pair++) {
	    cellerror = CELLOK;
	    if (cpairs[pair] && cpairs[pair]->expr) {
		v = (int) eval(cpairs[pair]->expr);
		cpairs[pair]->fg = v & 7;
		cpairs[pair]->bg = (v >> 3) & 7;
		init_pair(pair + 1, cpairs[pair]->fg, cpairs[pair]->bg);
	    }
	    /* Can't see to fix the problem if color 1 has an error, so
	     * turn off color in that case.
	     */
	    if (pair == 0 && cellerror) {
		color = 0;
		attron(COLOR_PAIR(0));
		color_set(0, NULL);
		error("Error in color 1: color turned off");
	    }
	}
    }

    (void) signal(SIGFPE, doquit);
}

/*
 * Evaluate all cells which have expressions and alter their numeric or string
 * values.  Return the number of cells which changed.
 */

static int 
RealEvalAll(void)
{
    register int i,j;
    int chgct = 0;
    register struct ent *p;

    if (calc_order == BYROWS) {
	for (i=0; i<=maxrow; i++)
	    for (j=0; j<=maxcol; j++)
		if ((p = *ATBL(tbl,i,j)) && p->expr)
		    RealEvalOne(p,i,j,&chgct);
    }
    else if (calc_order == BYCOLS) {
	for (j=0; j<=maxcol; j++) {
	    for (i=0; i<=maxrow; i++)
		if ((p = *ATBL(tbl,i,j)) && p->expr)
		    RealEvalOne(p,i,j,&chgct);
	}
    }
    else error("Internal error calc_order");
 
    return (chgct);
}

void
RealEvalOne(register struct ent *p, int i, int j, int *chgct)
{
    gmyrow=i; gmycol=j;

    if (p->flags & IS_STREXPR) {
	char *v;
	if (setjmp(fpe_save)) {
	    error("Floating point exception %s", v_name(i, j));
	    cellerror = CELLERROR;
	    v = "";
	} else {
	    cellerror = CELLOK;
	    v = seval(p->expr);
	}
	p->cellerror = cellerror;
	if (!v && !p->label) /* Everything's fine */
	    return;
	if (!p->label || !v || strcmp(v, p->label) != 0 || cellerror) {
	    (*chgct)++;
	    p->flags |= IS_CHANGED;
	    changed++;
	}
	if (p->label)
	    scxfree(p->label);
	p->label = v;
    } else {
	double v;
	if (setjmp(fpe_save)) {
	    error("Floating point exception %s", v_name(i, j));
	    cellerror = CELLERROR;
	    v = (double)0.0;
	} else {
	    cellerror = CELLOK;
	    v = eval(p->expr);
	    if (cellerror == CELLOK && !
#ifdef HAVE_ISFINITE
	      isfinite(
#else
	      finite(
#endif
	      v))
		cellerror = CELLERROR;
	}
	if ((cellerror != p->cellerror) || (v != p->v)) {
	    p->cellerror = cellerror;
	    p->v = v;
	    if (!cellerror)	/* don't keep eval'ing an error */
		(*chgct)++;
	    p->flags |= IS_CHANGED|IS_VALID;
	    changed++;
	}
    }
}

struct enode *
new(int op, struct enode *a1, struct enode *a2)
{
    register struct enode *p;
    if (freeenodes) {
    	p = freeenodes;
	freeenodes = p->e.o.left;
    } else
	p = scxmalloc(sizeof(struct enode));
    p->op = op;
    p->e.o.left = a1;
    p->e.o.right = a2;
    p->e.o.s = NULL;
    return p;
}

struct enode *
new_var(int op, struct ent_ptr a1)
{
    register struct enode *p;
    if (freeenodes) {
    	p = freeenodes;
	freeenodes = p->e.o.left;
    } else
	p = scxmalloc(sizeof(struct enode));
    p->op = op;
    p->e.v = a1;
    return p;
}

struct enode *
new_range(int op, struct range_s a1)
{
    register struct enode *p;
    if (freeenodes)
    {	p = freeenodes;
	freeenodes = p->e.o.left;
    }
    else
	p = scxmalloc(sizeof(struct enode));
    p->op = op;
    p->e.r = a1;
    return p;
}

struct enode *
new_const(int op, double a1)
{
    register struct enode *p;
    if (freeenodes) {	/* reuse an already free'd enode */
	p = freeenodes;
	freeenodes = p->e.o.left;
    } else
	p = scxmalloc(sizeof(struct enode));
    p->op = op;
    p->e.k = a1;
    return p;
}

struct enode *
new_str(char *s)
{
    register struct enode *p;

    if (freeenodes) {	/* reuse an already free'd enode */
    	p = freeenodes;
	freeenodes = p->e.o.left;
    } else
	p = scxmalloc(sizeof(struct enode));
    p->op = O_SCONST;
    p->e.s = s;
    return (p);
}

void
copy(struct ent *dv1, struct ent *dv2, struct ent *v1, struct ent *v2)
{
    struct ent *p;
    static int minsr = -1, minsc = -1;
    static int maxsr = -1, maxsc = -1;
    int mindr, mindc;
    int maxdr, maxdc;
    int r, c;
    int deltar, deltac;

    if (dv1) {
	mindr = dv1->row;
	mindc = dv1->col;
	maxdr = dv2->row;
	maxdc = dv2->col;
	if (mindr > maxdr) r = maxdr, maxdr = mindr, mindr = r;
	if (mindc > maxdc) c = maxdc, maxdc = mindc, mindc = c;
    } else {
	if (showrange) {
	    showrange = 0;
	    mindr = showsr < currow ? showsr : currow;
	    mindc = showsc < curcol ? showsc : curcol;
	    maxdr = showsr > currow ? showsr : currow;
	    maxdc = showsc > curcol ? showsc : curcol;
	} else if (v1) {
	    /* Set up the default source range for the "c." command. */
	    minsr = maxsr = v1->row;
	    minsc = maxsc = v1->col;
	    return;
	} else {
	    mindr = maxdr = currow;
	    mindc = maxdc = curcol;
	}
    }

    if (v1) {
	minsr = v1->row;
	minsc = v1->col;
	maxsr = v2->row;
	maxsc = v2->col;
	if (minsr > maxsr) r = maxsr, maxsr = minsr, minsr = r;
	if (minsc > maxsc) c = maxsc, maxsc = minsc, minsc = c;
    } else if (dv1 == NULL || v2 != NULL) {
	if (qbuf && delbuf[qbuf]) {
	    delbuf[++dbidx] = delbuf[qbuf];
	    delbuffmt[dbidx] = delbuffmt[qbuf];
	} else if (dbidx < 0) return;
	minsr = maxrow;
	minsc = maxcol;
	maxsr = 0;
	maxsc = 0;
	for (p = delbuf[dbidx]; p; p = p->next) {
	    if (p->row < minsr) minsr = p->row;
	    if (p->row > maxsr) maxsr = p->row;
	    if (p->col < minsc) minsc = p->col;
	    if (p->col > maxsc) maxsc = p->col;
	}
    } else if (showrange && !(showsr == currow && showsc == curcol &&
	    mindr == currow && mindc == curcol &&
	    maxdr == currow && maxdc == curcol)) {
	minsr = showsr < currow ? showsr : currow;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = showsc > curcol ? showsc : curcol;
    } else
	if (minsr == -1) return;

    checkbounds(&maxdr, &maxdc);

    if (maxdr - mindr < maxsr - minsr) maxdr = mindr + (maxsr - minsr);
    if (maxdc - mindc < maxsc - minsc) maxdc = mindc + (maxsc - minsc);
    if (dv1 && (v1 || !v2))
	yank_area(minsr, minsc, maxsr, maxsc);
    erase_area(mindr, mindc, maxdr, maxdc, 0);
    sync_refs();
    flush_saved();

    error("Copying...");
    if (!loading)
	refresh();
    p = delbuf[dbidx];
    if (minsr == maxsr && minsc == maxsc) {
	/* Source is a single cell */
	for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++)
	    for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
		copydbuf(deltar, deltac);
    } else if (minsr == maxsr) {
	/* Source is a single row */
	deltac = mindc - p->col;
	for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++)
	    copydbuf(deltar, deltac);
    } else if (minsc == maxsc) {
	/* Source is a single column */
	deltar = mindr - p->row;
	for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
	    copydbuf(deltar, deltac);
    } else {
	/* Everything else */
	deltar = mindr - p->row;
	deltac = mindc - p->col;
	copydbuf(deltar, deltac);
    }

    if (dv1 && (v1 || !v2)) {
	sync_refs();
	flush_saved();
    }

    if (dv1 == NULL) {
	if (qbuf && delbuf[qbuf]) {
	    delbuf[dbidx] = NULL;
	    delbuffmt[dbidx--] = NULL;
	}
	qbuf = 0;
    }
    error("Copy done.");
}

void
copydbuf(int deltar, int deltac)
{
    int vr, vc;
    struct ent *p = delbuf[dbidx];
    struct ent *n;

    while (p) {
	vr = p->row + deltar;
	vc = p->col + deltac;
	n = lookat(vr, vc);
	if (n->flags&IS_LOCKED) continue;
	copyent(n, p, deltar, deltac, 0, 0, maxrow, maxcol, 0);
	p = p->next;
    }
}

/* ERASE a Range of cells */
void
eraser(struct ent *v1, struct ent *v2)
{
    int i;
    struct ent *obuf = NULL;

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
    erase_area(v1->row, v1->col, v2->row, v2->col, 0);
    sync_refs();
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
    FullUpdate++;
    modflg++;
}

/* YANK a Range of cells */
void
yankr(struct ent *v1, struct ent *v2)
{
    int i, qtmp;
    struct ent *obuf = NULL;

    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
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
    yank_area(v1->row, v1->col, v2->row, v2->col);
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

/* MOVE a Range of cells */
void
mover(struct ent *d, struct ent *v1, struct ent *v2)
{
    move_area(d->row, d->col, v1->row, v1->col, v2->row, v2->col);
    sync_refs();
    FullUpdate++;
}

/* Goto subroutines */

static void
g_free(void)
{
    switch (gs.g_type) {
	case G_STR:
	case G_NSTR: scxfree(gs.g_s); break;
	default: break;
    }
    gs.g_type = G_NONE;
    gs.errsearch = 0;
}

/* repeat the last goto command */
void
go_last(void) {
    int num = 0;

    switch (gs.g_type) {
	case G_NONE:
	    error("Nothing to repeat"); break;
	case G_NUM:
	    num_search(gs.g_n, gs.g_row, gs.g_col,
		gs.g_lastrow, gs.g_lastcol, gs.errsearch);
	    break;
	case G_CELL:
	    moveto(gs.g_row, gs.g_col, gs.g_lastrow, gs.g_lastcol,
		gs.strow, gs.stcol);
	    break;
	case G_XSTR: 
	    num++;
	case G_NSTR: 
	    num++;
	case G_STR: 
	    gs.g_type = G_NONE;	/* Don't free the string */
	    str_search(gs.g_s, gs.g_row, gs.g_col, gs.g_lastrow, gs.g_lastcol,
		    num); 
	    break;

	default: error("go_last: internal error");
    }
}

/* Place the cursor on a given cell.  If cornerrow >= 0, place the cell
 * at row cornerrow and column cornercol in the upper left corner of the
 * screen if possible.
 */
void
moveto(int row, int col, int lastrow, int lastcol, int cornerrow,
    int cornercol)
{
    register int i;

    if (!loading && row != -1 && (row != currow || col != curcol))
	remember(0);

    currow = row;
    curcol = col;
    g_free();
    gs.g_type = G_CELL;
    gs.g_row = currow;
    gs.g_col = curcol;
    gs.g_lastrow = lastrow;
    gs.g_lastcol = lastcol;
    gs.strow = cornerrow;
    gs.stcol = cornercol;
    if (cornerrow >= 0) {
	strow = cornerrow;
	stcol = cornercol;
	gs.stflag = 1;
    } else
	gs.stflag = 0;
    for (rowsinrange = 0, i = row; i <= lastrow; i++) {
	if (row_hidden[i])
	    continue;
	rowsinrange++;
    }
    for (colsinrange = 0, i = col; i <= lastcol; i++) {
	if (col_hidden[i])
	    continue;
	colsinrange += fwidth[i];
    }
    FullUpdate++;
    if (loading) {
	update(1);
	changed = 0;
    } else
	remember(1);
}

/*
 * 'goto' either a given number,'error', or 'invalid' starting at currow,curcol
 */
void
num_search(double n, int firstrow, int firstcol, int lastrow,
    int lastcol, int errsearch)
{
    register struct ent *p;
    register int r,c;
    int	endr, endc;

    if (!loading)
	remember(0);

    g_free();
    gs.g_type = G_NUM;
    gs.g_n = n;
    gs.g_row = firstrow;
    gs.g_col = firstcol;
    gs.g_lastrow = lastrow;
    gs.g_lastcol = lastcol;
    gs.errsearch = errsearch;

    if (currow >= firstrow && currow <= lastrow &&
	    curcol >= firstcol && curcol <= lastcol) {
	endr = currow;
	endc = curcol;
    } else {
	endr = lastrow;
	endc = lastcol;
    }
    r = endr;
    c = endc;
    while (1) {
	if (c < lastcol)
	    c++;
	else {
	    if (r < lastrow) {
		while (++r < lastrow && row_hidden[r]) /* */;
		c = firstcol;
	    } else {
		r = firstrow;
		c = firstcol;
	    }
	}
	p = *ATBL(tbl, r, c);
	if (!col_hidden[c] && p && (p->flags & IS_VALID) &&
		(errsearch || (p->v == n)) && (!errsearch ||
		(p->cellerror == errsearch)))	/* CELLERROR vs CELLINVALID */
	    break;
	if (r == endr && c == endc) {
	    if (errsearch) {
		error("no %s cell found", errsearch == CELLERROR ? "ERROR" :
		      "INVALID");
	    } else {
		error("Number not found");
	    }
	    return;
	}
    }
		
    currow = r;
    curcol = c;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    if (loading) {
	update(1);
	changed = 0;
    } else
	remember(1);
}

/* 'goto' a cell containing a matching string */
void
str_search(char *s, int firstrow, int firstcol, int lastrow, int lastcol,
	int num)
{
    struct ent	*p;
    int		r, c;
    int		endr, endc;
    char	*tmp;
#if defined(REGCOMP)
    regex_t	preg;
    int		errcode;
#endif

    if (!loading)
	remember(0);

#if defined(REGCOMP)
    if ((errcode = regcomp(&preg, s, REG_EXTENDED))) {
	scxfree(s);
	tmp = scxmalloc(160);
	regerror(errcode, &preg, tmp, sizeof(tmp));
	error("%s", tmp);
	scxfree(tmp);
	return;
    }
#endif
#if defined(RE_COMP)
    if ((tmp = re_comp(s)) != NULL) {
	scxfree(s);
	error("%s", tmp);
	return;
    }
#endif
#if defined(REGCMP)
    if ((tmp = regcmp(s, NULL)) == NULL) {
	scxfree(s);
	cellerror = CELLERROR;
	error("Invalid search string");
	return;
    }
#endif
    g_free();
    gs.g_type = G_STR + num;
    gs.g_s = s;
    gs.g_row = firstrow;
    gs.g_col = firstcol;
    gs.g_lastrow = lastrow;
    gs.g_lastcol = lastcol;
    if (currow >= firstrow && currow <= lastrow &&
	    curcol >= firstcol && curcol <= lastcol) {
	endr = currow;
	endc = curcol;
    } else {
	endr = lastrow;
	endc = lastcol;
    }
    r = endr;
    c = endc;
    while (1) {
	if (c < lastcol) {
	    c++;
	} else {
	    if (r < lastrow) {
		while (++r < lastrow && row_hidden[r]) /* */;
		c = firstcol;
	    } else {
		r = firstrow;
		c = firstcol;
	    }
	}
	p = *ATBL(tbl, r, c);
	if (gs.g_type == G_NSTR) {
	    *line = '\0';
	    if (p) {
		if (p->cellerror)
		    snprintf(line, sizeof line, "%s", p->cellerror == CELLERROR ?
			    "ERROR" : "INVALID");
		else if (p->flags & IS_VALID) {
		    if (p->format) {
			if (*(p->format) == ctl('d')) {
			    time_t i = (time_t) (p->v);
			    strftime(line, sizeof(line), (p->format)+1,
				localtime(&i));
			} else
			    format(p->format, precision[c], p->v, line,
				    sizeof(line));
		    } else
			engformat(realfmt[c], fwidth[c], precision[c],
				p->v, line, sizeof(line));
		}
	    }
	} else if (gs.g_type == G_XSTR) {
	    *line = '\0';
	    if (p && p->expr) {
		linelim = 0;
		decompile(p->expr, 0);	/* set line to expr */
		line[linelim] = '\0';
		if (*line == '?')
		    *line = '\0';
	    }
	}
	if (!col_hidden[c]) {
	    if (gs.g_type == G_STR) {
		if (p && p->label
#if defined(REGCOMP)
			&& (regexec(&preg, p->label, 0, NULL, 0) == 0))
#else
#if defined(RE_COMP)
			&& (re_exec(p->label) != 0))
#else
#if defined(REGCMP)
			&& (regex(tmp, p->label) != (char *)0))
#else
			&& (strcmp(s, p->label) == 0))
#endif
#endif
#endif
		    break;
	    } else			/* gs.g_type != G_STR */
		if (*line != '\0'
#if defined(REGCOMP)
			&& (regexec(&preg, line, 0, NULL, 0) == 0))
#else
#if defined(RE_COMP)
			&& (re_exec(line) != 0))
#else
#if defined(REGCMP)
			&& (regex(tmp, line) != (char *)0))
#else
			&& (strcmp(s, line) == 0))
#endif
#endif
#endif
		    break;
	}
	if (r == endr && c == endc) {
	    error("String not found");
#if defined(REGCOMP)
	    regfree(&preg);
#endif
#if defined(REGCMP)
	    free(tmp);
#endif
	    linelim = -1;
	    return;
	}
    }
    linelim = -1;
    currow = r;
    curcol = c;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
#if defined(REGCOMP)
    regfree(&preg);
#endif
#if defined(REGCMP)
    free(tmp);
#endif
    if (loading) {
	update(1);
	changed = 0;
    } else
	remember(1);
}

/* fill a range with constants */
void
fill(struct ent *v1, struct ent *v2, double start, double inc)
{
    int r, c;
    register struct ent *n;
    int maxr, maxc;
    int minr, minc;

    maxr = v2->row;
    maxc = v2->col;
    minr = v1->row;
    minc = v1->col;
    if (minr>maxr) r = maxr, maxr = minr, minr = r;
    if (minc>maxc) c = maxc, maxc = minc, minc = c;
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    FullUpdate++;
    if (calc_order == BYROWS) {
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    n = lookat(r, c);
	    if (n->flags&IS_LOCKED) continue;
	    (void) clearent(n);
	    n->v = start;
	    start += inc;
	    n->flags |= (IS_CHANGED|IS_VALID);
	    n->flags &= ~(IS_CLEARED);
	}
    }
    else if (calc_order == BYCOLS) {
    for (c = minc; c<=maxc; c++)
	for (r = minr; r<=maxr; r++) {
	    n = lookat(r, c);
	    (void) clearent(n);
	    n->v = start;
	    start += inc;
	    n->flags |= (IS_CHANGED|IS_VALID);
	    n->flags &= ~(IS_CLEARED);
	}
    }
    else error(" Internal error calc_order");
    changed++;
}

/* lock a range of cells */

void
lock_cells(struct ent *v1, struct ent *v2)
{
    int r, c;
    register struct ent *n;
    int maxr, maxc;
    int minr, minc;

    maxr = v2->row;
    maxc = v2->col;
    minr = v1->row;
    minc = v1->col;
    if (minr>maxr) r = maxr, maxr = minr, minr = r;
    if (minc>maxc) c = maxc, maxc = minc, minc = c;
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    n = lookat(r, c);
	    n->flags |= IS_LOCKED;
	}
}

/* unlock a range of cells */

void
unlock_cells(struct ent *v1, struct ent *v2)
{
    int r, c;
    register struct ent *n;
    int maxr, maxc;
    int minr, minc;

    maxr = v2->row;
    maxc = v2->col;
    minr = v1->row;
    minc = v1->col;
    if (minr>maxr) r = maxr, maxr = minr, minr = r;
    if (minc>maxc) c = maxc, maxc = minc, minc = c;
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    n = lookat(r, c);
	    n->flags &= ~IS_LOCKED;
	}
}

/* set the numeric part of a cell */
void
let(struct ent *v, struct enode *e)
{
    double val;
    unsigned isconstant = constant(e);

    if (locked_cell(v->row, v->col))
	return;
    if (v->row == currow && v->col == curcol)
	cellassign = 1;
    if (loading && !isconstant)
	val = (double)0.0;
    else {
	exprerr = 0;
	(void) signal(SIGFPE, eval_fpe);
	if (setjmp(fpe_save)) {
	    error("Floating point exception in cell %s", v_name(v->row, v->col));
	    val = (double)0.0;
	    cellerror = CELLERROR;
	} else {
	    cellerror = CELLOK;
	    val = eval(e);
	}
	if (v->cellerror != cellerror) {
	    v->flags |= IS_CHANGED;
	    changed++;
	    modflg++;
	    FullUpdate++;
	    v->cellerror = cellerror;
	}
	(void) signal(SIGFPE, doquit);
	if (exprerr) {
	    efree(e);
	    return;
	}
    }

    if (isconstant) {
	/* prescale input unless it has a decimal */
	if (!loading && !decimal && (prescale < (double)0.9999999))
	    val *= prescale;
	decimal = FALSE;

	v->v = val;

	if (!(v->flags & IS_STREXPR)) {
            efree(v->expr);
	    v->expr = (struct enode *)0;
	}
	efree(e);
    } else {
	efree(v->expr);
	v->expr = e;
	v->flags &= ~IS_STREXPR;
    }

    changed++; modflg++;
    v->flags |= (IS_CHANGED|IS_VALID);
    
    if (!loading) {
	int i;

	for (i = 36; i > 28; i--) {
	    savedrow[i] = savedrow[i-1];
	    savedcol[i] = savedcol[i-1];
	    savedstrow[i] = savedstrow[i-1];
	    savedstcol[i] = savedstcol[i-1];
	}

	savedrow[28] = v->row;
	savedcol[28] = v->col;
	savedstrow[28] = savedstrow[27];
	savedstcol[28] = savedstcol[27];
    }
}

void
slet(struct ent *v, struct enode *se, int flushdir)
{
    char *p;

    if (locked_cell(v->row, v->col))
	return;
    if (v->row == currow && v->col == curcol)
	cellassign = 1;
    exprerr = 0;
    (void) signal(SIGFPE, eval_fpe);
    if (setjmp(fpe_save)) {
	error ("Floating point exception in cell %s", v_name(v->row, v->col));
	cellerror = CELLERROR;
	p = "";
    } else {
	cellerror = CELLOK;
	p = seval(se);
    }
    if (v->cellerror != cellerror) {
    	v->flags |= IS_CHANGED;
	changed++;	modflg++;
	FullUpdate++;
	v->cellerror = cellerror;
    }
    (void) signal(SIGFPE, doquit);
    if (exprerr) {
	efree(se);
	return;
    }
    if (!loading) {
	int i;

	for (i = 36; i > 28; i--) {
	    savedrow[i] = savedrow[i-1];
	    savedcol[i] = savedcol[i-1];
	    savedstrow[i] = savedstrow[i-1];
	    savedstcol[i] = savedstcol[i-1];
	}

	savedrow[28] = v->row;
	savedcol[28] = v->col;
	savedstrow[28] = savedstrow[27];
	savedstcol[28] = savedstcol[27];
    }
    if (constant(se)) {
	label(v, p, flushdir);
	if (p)
	    scxfree(p);
	efree(se);
	if (v->flags & IS_STREXPR) {
            efree(v->expr);
	    v->expr = (struct enode *)0;
	    v->flags &= ~IS_STREXPR;
	}
	return;
    }
    efree(v->expr);
    v->expr = se;
    v->flags |= (IS_CHANGED|IS_STREXPR);
    if (flushdir < 0) v->flags |= IS_LEFTFLUSH;

    if (flushdir == 0)
	v->flags |= IS_LABEL;
    else v->flags &= ~IS_LABEL;

    FullUpdate++;
    changed++;
    modflg++;
}

void
format_cell(struct ent *v1, struct ent *v2, char *s)
{
    int r, c;
    register struct ent *n;
    int maxr, maxc;
    int minr, minc;
    size_t l;

    maxr = v2->row;
    maxc = v2->col;
    minr = v1->row;
    minc = v1->col;
    if (minr>maxr) r = maxr, maxr = minr, minr = r;
    if (minc>maxc) c = maxc, maxc = minc, minc = c;
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    FullUpdate++;
    modflg++;

    for (r = minr; r <= maxr; r++)
	for (c = minc; c <= maxc; c++) {
	    n = lookat(r, c);
	    if (locked_cell(n->row, n->col))
		continue;
	    if (n->format)
		scxfree(n->format);
	    n->format = 0;
	    if (s && *s != '\0') {
		l = strlen(s) + 1;
		n->format = scxmalloc(l);
		strlcpy(n->format, s, l);
	    }
	    n->flags |= IS_CHANGED;
	}
}

void
hide_row(int a)
{
    if (a < 0) {
	error("Invalid Range");
	return;
    }
    if (a >= maxrows-1) {
	if (!growtbl(GROWROW, a+1, 0)) {
	    error("You can't hide the last row");
	    return;
	}
    }
    FullUpdate++;
    row_hidden[a] = TRUE;
}

void
hide_col(int a)
{
    if (a < 0) {
	error("Invalid Range");
	return;
    }
    if (a >= maxcols-1) {
    	if ((a >= ABSMAXCOLS-1) || !growtbl(GROWCOL, 0, a+1)) {
	    error("You can't hide the last col");
	    return;
	}
    }
    FullUpdate++;
    col_hidden[a] = TRUE;
}

void
clearent(struct ent *v)
{
    if (!v)
	return;
    label(v,"",-1);
    v->v = (double)0;
    if (v->expr)
	efree(v->expr);
    v->expr = NULL;
    if (v->format)
	scxfree(v->format);
    v->format = NULL;
    v->flags = (IS_CHANGED|IS_CLEARED);
    changed++;
    modflg++;
}

/*
 * Say if an expression is a constant (return 1) or not.
 */
int
constant(register struct enode *e)
{
    return (
	 e == NULL
	 || e->op == O_CONST
	 || e->op == O_SCONST
	 || (e->op == 'm' && constant(e->e.o.left))
	 || (
	     e->op != O_VAR
	     && !(e->op & REDUCE)
	     && constant(e->e.o.left)
	     && constant(e->e.o.right)
	     && e->op != EXT	 /* functions look like constants but aren't */
	     && e->op != NVAL
	     && e->op != SVAL
	     && e->op != NOW
	     && e->op != MYROW
	     && e->op != MYCOL
	     && e->op != LASTROW
	     && e->op != LASTCOL
	     && e->op != NUMITER
	     && e->op != FILENAME
             && optimize
	)
    );
}

void
efree(struct enode *e)
{
    if (e) {
	if (e->op != O_VAR && e->op != O_CONST && e->op != O_SCONST
		&& !(e->op & REDUCE)) {
	    efree(e->e.o.left);
	    efree(e->e.o.right);
	}
	if (e->op == O_SCONST && e->e.s)
	    scxfree(e->e.s);
	else if (e->op == EXT && e->e.o.s)
	    scxfree(e->e.o.s);
	e->e.o.left = freeenodes;
	freeenodes = e;
    }
}

void
label(register struct ent *v, register char *s, int flushdir)
{
    if (v) {
	if (flushdir==0 && v->flags&IS_VALID) {
	    register struct ent *tv;
	    if (v->col>0 && ((tv=lookat(v->row,v->col-1))->flags&IS_VALID)==0)
		v = tv, flushdir = 1;
	    else if (((tv=lookat(v->row,v->col+1))->flags&IS_VALID)==0)
		v = tv, flushdir = -1;
	    else flushdir = -1;
	}
	if (v->label) scxfree((char *)(v->label));
	if (s && s[0]) {
	    size_t l = strlen(s) + 1;
	    v->label = scxmalloc(l);
	    strlcpy(v->label, s, l);
	} else
	    v->label = (char *)0;
	if (flushdir<0) v->flags |= IS_LEFTFLUSH;
	else v->flags &= ~IS_LEFTFLUSH;
	if (flushdir==0) v->flags |= IS_LABEL;
	else v->flags &= ~IS_LABEL;
	FullUpdate++;
	modflg++;
    }
}

static void
decodev(struct ent_ptr v)
{
	struct range *r;

	if (!v.vp || v.vp->flags & IS_DELETED)
	    snprintf(line + linelim, sizeof(line) - linelim, "@ERR");
	else if (!find_range((char *)0, 0, v.vp, v.vp, &r) && !r->r_is_range)
	    snprintf(line + linelim, sizeof(line) - linelim, "%s", r->r_name);
	else {
	    snprintf(line + linelim, sizeof(line) - linelim, "%s%s%s%d",
		v.vf & FIX_COL ? "$" : "",
		coltoa(v.vp->col),
		v.vf & FIX_ROW ? "$" : "",
		v.vp->row);
	}
	linelim += strlen(line+linelim);
}

char *
coltoa(int col)
{
    static char rname[3];
    register char *p = rname;

    if (col > 25) {
	*p++ = col/26 + 'A' - 1;
	col %= 26;
    }
    *p++ = col+'A';
    *p = '\0';
    return (rname);
}

/*
 *	To make list elements come out in the same order
 *	they were entered, we must do a depth-first eval
 *	of the ELIST tree
 */
static void
decompile_list(struct enode *p)
{
	if (!p) return;
	decompile_list(p->e.o.left);	/* depth first */
        decompile(p->e.o.right, 0);
	line[linelim++] = ',';
}

void
decompile(register struct enode *e, int	priority)
{
    register char *s;
    if (e) {
	int mypriority;
	switch (e->op) {
	default: mypriority = 99; break;
	case ';': mypriority = 1; break;
	case '?': mypriority = 2; break;
	case ':': mypriority = 3; break;
	case '|': mypriority = 4; break;
	case '&': mypriority = 5; break;
	case '<': case '=': case '>': mypriority = 6; break;
	case '+': case '-': case '#': mypriority = 8; break;
	case '*': case '/': case '%': mypriority = 10; break;
	case '^': mypriority = 12; break;
	}
	if (mypriority<priority) line[linelim++] = '(';
	switch (e->op) {
	case 'f':	for (s="@fixed "; (line[linelim++] = *s++); )
				;
			linelim--;
			decompile(e->e.o.left, 30);
			break;
	case 'F':	for (s="(@fixed)"; (line[linelim++] = *s++); )
				;
			linelim--;
			decompile(e->e.o.left, 30);
			break;
	case 'm':	line[linelim++] = '-';
			decompile(e->e.o.left, 30);
			break;
	case '!':	line[linelim++] = '!';
			decompile(e->e.o.left, 30);
			break;
	case O_VAR:	decodev(e->e.v);
			break;
	case O_CONST:	snprintf(line + linelim, sizeof(line) - linelim,
			    "%.15g", e->e.k);
			linelim += strlen(line+linelim);
			break;
	case O_SCONST:	snprintf(line + linelim, sizeof(line) - linelim,
			    "\"%s\"", e->e.s);
			linelim += strlen(line+linelim);
			break;

	case SUM	: index_arg("@sum", e); break;
	case PROD	: index_arg("@prod", e); break;
	case AVG	: index_arg("@avg", e); break;
	case COUNT	: index_arg("@count", e); break;
	case STDDEV	: index_arg("@stddev", e); break;
	case MAX	: index_arg("@max", e); break;
	case MIN	: index_arg("@min", e); break;
	case REDUCE | 'R': range_arg("@rows(", e); break;
	case REDUCE | 'C': range_arg("@cols(", e); break;

	case ABS:	one_arg("@abs(", e); break;
	case ACOS:	one_arg("@acos(", e); break;
	case ASIN:	one_arg("@asin(", e); break;
	case ATAN:	one_arg("@atan(", e); break;
	case ATAN2:	two_arg("@atan2(", e); break;
	case CEIL:	one_arg("@ceil(", e); break;
	case COS:	one_arg("@cos(", e); break;
	case EXP:	one_arg("@exp(", e); break;
	case FABS:	one_arg("@fabs(", e); break;
	case FLOOR:	one_arg("@floor(", e); break;
	case HYPOT:	two_arg("@hypot(", e); break;
	case LOG:	one_arg("@ln(", e); break;
	case LOG10:	one_arg("@log(", e); break;
	case POW:	two_arg("@pow(", e); break;
	case SIN:	one_arg("@sin(", e); break;
	case SQRT:	one_arg("@sqrt(", e); break;
	case TAN:	one_arg("@tan(", e); break;
	case DTR:	one_arg("@dtr(", e); break;
	case RTD:	one_arg("@rtd(", e); break;
	case RND:	one_arg("@rnd(", e); break;
	case ROUND:	two_arg("@round(", e); break;
	case HOUR:	one_arg("@hour(", e); break;
	case MINUTE:	one_arg("@minute(", e); break;
	case SECOND:	one_arg("@second(", e); break;
	case MONTH:	one_arg("@month(", e); break;
	case DAY:	one_arg("@day(", e); break;
	case YEAR:	one_arg("@year(", e); break;
	case NOW:	for (s = "@now"; (line[linelim++] = *s++); )
				;
			linelim--;
			break;
	case DATE:	if (e->e.o.right)
			    two_arg("@date(", e);
			else
			    one_arg("@date(", e);
			break;
	case FMT:	two_arg("@fmt(", e); break;
	case UPPER:	one_arg("@upper(", e); break;
	case LOWER:	one_arg("@lower(", e); break;
	case CAPITAL:	one_arg("@capital(", e); break;
	case DTS:	three_arg("@dts(", e); break;
	case TTS:	three_arg("@tts(", e); break;
	case STON:	one_arg("@ston(", e); break;
	case EQS:	two_arg("@eqs(", e); break;
	case LMAX:	list_arg("@max(", e); break;
	case LMIN: 	list_arg("@min(", e); break;
	case FV:	three_arg("@fv(", e); break;
	case PV:	three_arg("@pv(", e); break;
	case PMT:	three_arg("@pmt(", e); break;
	case NVAL:	two_arg("@nval(", e); break;
	case SVAL:	two_arg("@sval(", e); break;
	case EXT:	two_arg("@ext(", e); break;
	case SUBSTR:	three_arg("@substr(", e); break;
	case STINDEX:	index_arg("@stindex", e); break;
	case INDEX:	index_arg("@index", e); break;
	case LOOKUP:	index_arg("@lookup", e); break;
	case HLOOKUP:	two_arg_index("@hlookup", e); break;
	case VLOOKUP:	two_arg_index("@vlookup", e); break;
	case IF:	three_arg("@if(", e); break;
	case MYROW:	for (s = "@myrow"; (line[linelim++] = *s++); )
				;
			linelim--;
			break;
	case MYCOL:	for (s = "@mycol"; (line[linelim++] = *s++); )
				;
			linelim--;
			break;
	case LASTROW:	for (s = "@lastrow"; (line[linelim++] = *s++); )
				;
			linelim--;
			break;
	case LASTCOL:	for (s = "@lastcol"; (line[linelim++] = *s++); )
				;
			linelim--;
			break;
	case COLTOA:	one_arg( "@coltoa(", e); break;
	case FILENAME:	one_arg( "@filename(", e); break;
	case NUMITER:	for (s = "@numiter"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case ERR_:	for (s = "@err"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case PI_:	for (s = "@pi"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case BLACK:	for (s = "@black"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case RED:	for (s = "@red"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case GREEN:	for (s = "@green"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case YELLOW:	for (s = "@yellow"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case BLUE:	for (s = "@blue"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case MAGENTA:	for (s = "@magenta"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case CYAN:	for (s = "@cyan"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	case WHITE:	for (s = "@white"; (line[linelim++] = *s++); )
				;
                        linelim--;
                        break;
	default:	decompile(e->e.o.left, mypriority);
			line[linelim++] = e->op;
			decompile(e->e.o.right, mypriority+1);
			break;
	}
	if (mypriority<priority) line[linelim++] = ')';
    } else line[linelim++] = '?';
}

void
index_arg(char *s, struct enode *e)
{
    if (e->e.o.right && e->e.o.right->op == ',') {
	two_arg_index(s, e);
	return;
    }
    for (; (line[linelim++] = *s++); );
    linelim--;
    range_arg("(", e->e.o.left);
    linelim--;
    if (e->e.o.right) {
	line[linelim++] = ',';
	decompile(e->e.o.right, 0);
    }
    line[linelim++] = ')';
}

void
two_arg_index(char *s, struct enode *e)
{
    for (; (line[linelim++] = *s++); );
    linelim--;
    range_arg("(", e->e.o.left);
    linelim--;
    line[linelim++] = ',';
    decompile(e->e.o.right->e.o.left, 0);
    line[linelim++] = ',';
    decompile(e->e.o.right->e.o.right, 0);
    line[linelim++] = ')';
}

void
list_arg(char *s, struct enode *e)
{
    for (; (line[linelim++] = *s++); );
    linelim--;

    decompile(e->e.o.right, 0);
    line[linelim++] = ',';
    decompile_list(e->e.o.left);
    line[linelim - 1] = ')';
}

void
one_arg(char *s, struct enode *e)
{
    for (; (line[linelim++] = *s++); );
    linelim--;
    decompile(e->e.o.left, 0);
    line[linelim++] = ')';
}

void
two_arg(char *s, struct enode *e)
{
    for (; (line[linelim++] = *s++); );
    linelim--;
    decompile(e->e.o.left, 0);
    line[linelim++] = ',';
    decompile (e->e.o.right, 0);
    line[linelim++] = ')';
}

void
three_arg(char *s, struct enode *e)
{
    for (; (line[linelim++] = *s++); );
    linelim--;
    decompile(e->e.o.left, 0);
    line[linelim++] = ',';
    decompile(e->e.o.right->e.o.left, 0);
    line[linelim++] = ',';
    decompile (e->e.o.right->e.o.right, 0);
    line[linelim++] = ')';
}

void
range_arg(char *s, struct enode *e)
{
    struct range *r;

    for (; (line[linelim++] = *s++); );
    linelim--;
    if (!find_range((char *)0, 0, e->e.r.left.vp,
	    e->e.r.right.vp, &r) && r->r_is_range) {
	snprintf(line + linelim, sizeof(line) - linelim,
	    "%s", r->r_name);
	linelim += strlen(line+linelim);
    } else {
	decodev(e->e.r.left);
	line[linelim++] = ':';
	decodev(e->e.r.right);
    }
    line[linelim++] = ')';
}

void
editfmt(int row, int col)
{
    register struct ent *p;

    p = lookat(row, col);
    if (p->format) {
        snprintf(line, sizeof line,  "fmt %s \"%s\"", v_name(row, col), p->format);
	linelim = strlen(line);
    }
}

void
editv(int row, int col)
{
    register struct ent *p;

    p = lookat(row, col);
    snprintf(line, sizeof line, "let %s = ", v_name(row, col));
    linelim = strlen(line);
    if (p->flags & IS_VALID) {
	if (p->flags & IS_STREXPR || p->expr == NULL) {
	    snprintf(line + linelim, sizeof(line) - linelim,
	        "%.15g", p->v);
	    linelim = strlen(line);
	} else
	    editexp(row, col);
    }
}

void
editexp(int row, int col)
{
    register struct ent *p;

    p = lookat(row, col);
    decompile(p->expr, 0);
    line[linelim] = '\0';
}

void
edits(int row, int col)
{
    register struct ent *p;

    p = lookat(row, col);
    if (p->flags & IS_LABEL)
	snprintf(line, sizeof line, "label %s = ", v_name(row, col));
    else
	snprintf(line, sizeof line, "%sstring %s = ",
		((p->flags & IS_LEFTFLUSH) ? "left" : "right"),
		v_name(row, col));
    linelim = strlen(line);
    if (p->flags & IS_STREXPR && p->expr) {
	editexp(row, col);
    } else if (p->label) {
        snprintf(line + linelim, sizeof(line) - linelim, "\"%s\"", p->label);
        linelim += strlen(line+linelim);
    } else {
        snprintf(line + linelim, sizeof(line) - linelim, "\"");
        linelim += 1;
    }
}

#ifdef RINT
/*	round-to-even, also known as ``banker's rounding''.
	With round-to-even, a number exactly halfway between two values is
	rounded to whichever is even; e.g. rnd(0.5)=0, rnd(1.5)=2,
	rnd(2.5)=2, rnd(3.5)=4.  This is the default rounding mode for
	IEEE floating point, for good reason: it has better numeric
	properties.  For example, if X+Y is an integer,
	then X+Y = rnd(X)+rnd(Y) with round-to-even,
	but not always with sc's rounding (which is
	round-to-positive-infinity).  I ran into this problem when trying to
	split interest in an account to two people fairly.
*/

double
rint(double d)
{
	/* as sent */
	double fl = floor(d),  fr = d-fl;
	return
	    fr<0.5  ||  fr==0.5 && fl==floor(fl/2)*2   ?   fl   :   ceil(d);
}
#endif
