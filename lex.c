/*	SC	A Spreadsheet Calculator
 *		Lexical analyser
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3/88, see list of changes.
 *		$Revision: 7.16 $
 *
 */


#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <math.h>

#if defined(BSD42) || defined(BSD43)
#include <sys/ioctl.h>
#endif 

#ifdef USE_IEEEFP_H
# include <ieeefp.h>
#endif

#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <unistd.h>
#include <limits.h>
#include "compat.h"
#include "sc.h"

static void fpe_trap(int);

#ifdef VMS
# include "gram_tab.h"
typedef union {
    int ival;
    double fval;
    struct ent *ent;
    struct enode *enode;
    char *sval;
    struct range_s rval;
} YYSTYPE;
extern YYSTYPE yylval;
extern int VMS_read_raw;   /*sigh*/
#else	/* VMS */
# if defined(MSDOS)
#  include "y_tab.h"
# else
#  include "y.tab.h"
# endif /* MSDOS */
#endif /* VMS */

#ifdef hpux
extern YYSTYPE yylval;
#endif /* hpux */

jmp_buf wakeup;
jmp_buf fpe_buf;

bool decimal = FALSE;

static void
fpe_trap(int signo)
{
    (void)signo;
#if defined(i386) && !defined(M_XENIX)
    asm("	fnclex");
    asm("	fwait");
#else
# ifdef IEEE_MATH
    (void)fpsetsticky((fp_except)0);	/* Clear exception */
# endif /* IEEE_MATH */
# ifdef PC
    _fpreset();
# endif
#endif
    longjmp(fpe_buf, 1);
}

struct key {
    char *key;
    int val;
};

struct key experres[] = {
#include "experres.h"
    { 0, 0 }
};

struct key statres[] = {
#include "statres.h"
    { 0, 0 }
};

int
yylex(void)
{
    char *p = line + linelim;
    int ret = -1;
    static int isfunc = 0;
    static bool isgoto = 0;
    static bool colstate = 0;
    static int dateflag;
    static char *tokenst = NULL;
    static size_t tokenl;

    while (isspace((int)*p)) p++;
    if (*p == '\0') {
	isfunc = isgoto = 0;
	ret = -1;
    } else if (isalpha((int)*p) || (*p == '_')) {
	register char *la;	/* lookahead pointer */
	register struct key *tblp;

	if (!tokenst) {
	    tokenst = p;
	    tokenl = 0;
	}
	/*
	 *  This picks up either 1 or 2 alpha characters (a column) or
	 *  tokens made up of alphanumeric chars and '_' (a function or
	 *  token or command or a range name)
	 */
	while (isalpha((int)*p) && isascii((int)*p)) {
	    p++;
	    tokenl++;
	}
	la = p;
	while (isdigit((int)*la) || (*la == '$'))
	    la++;
	/*
	 * A COL is 1 or 2 char alpha with nothing but digits following
	 * (no alpha or '_')
	 */
	if (!isdigit((int)*tokenst) && tokenl && tokenl <= 2 && (colstate ||
		(isdigit((int)*(la-1)) && !(isalpha((int)*la) || (*la == '_'))))) {
	    ret = COL;
	    yylval.ival = atocol(tokenst, tokenl);
	} else {
	    while (isalpha((int)*p) || (*p == '_') || isdigit((int)*p)) {
		p++;
		tokenl++;
	    }
	    ret = WORD;
	    if (!linelim || isfunc) {
		if (isfunc) isfunc--;
		for (tblp = linelim ? experres : statres; tblp->key; tblp++)
		    if (((tblp->key[0] ^ tokenst[0]) & 0x5F) == 0) {
		    /* Commenting the following line makes the search slower */
		    /* but avoids access outside valid memory. A BST would   */
		    /* be the better alternative. */
		    /*  && tblp->key[tokenl] == 0) { */
			unsigned int i = 1;
			while (i < tokenl && ((tokenst[i] ^ tblp->key[i]) & 0x5F) == 0)
			    i++;
			if (i >= tokenl) {
			    ret = tblp->val;
			    colstate = (ret <= S_FORMAT);
			    if (isgoto) {
				isfunc = isgoto = 0;
				if (ret != K_ERROR && ret != K_INVALID)
				    ret = WORD;
			    }
			    break;
			}
		    }
	    }
	    if (ret == WORD) {
		struct range *r;
		char *path;
		if (!find_range(tokenst, tokenl, NULL, NULL, &r)) {
		    yylval.rval.left = r->r_left;
		    yylval.rval.right = r->r_right;
		    if (r->r_is_range)
		        ret = RANGE;
		    else
			ret = VAR;
		} else if ((path = scxmalloc(PATHLEN)) &&
			plugin_exists(tokenst, tokenl, path)) {
		    strlcat(path, p, PATHLEN);
		    yylval.sval = path;
		    ret = PLUGIN;
		} else {
		    scxfree(path);
		    linelim = p-line;
		    yyerror("Unintelligible word");
		}
	    }
	}
    } else if ((*p == '.') || isdigit((int)*p)) {
#ifdef SIGVOID
	void (*sig_save)(int signum);
#else
	int (*sig_save)(int signum);
#endif
	double v = 0.0;
	int temp;
	char *nstart = p;

	sig_save = signal(SIGFPE, fpe_trap);
	if (setjmp(fpe_buf)) {
	    (void) signal(SIGFPE, sig_save);
	    yylval.fval = v;
	    error("Floating point exception\n");
	    isfunc = isgoto = 0;
	    tokenst = NULL;
	    return FNUMBER;
	}

	if (*p=='.' && dateflag) {  /* .'s in dates are returned as tokens. */
	    ret = *p++;
	    dateflag--;
	} else {
	    if (*p != '.') {
		tokenst = p;
		tokenl = 0;
		do {
		    v = v*10.0 + (double) ((unsigned) *p - '0');
		    tokenl++;
		} while (isdigit((int)*++p));
		if (dateflag) {
		    ret = NUMBER;
		    yylval.ival = (int)v;
		/*
		 *  If a string of digits is followed by two .'s separated by
		 *  one or two digits, assume this is a date and return the
		 *  .'s as tokens instead of interpreting them as decimal
		 *  points.  dateflag counts the .'s as they're returned.
		 */
		} else if (*p=='.' && isdigit((int)*(p+1)) && (*(p+2)=='.' ||
			(isdigit((int)*(p+2)) && *(p+3)=='.'))) {
		    ret = NUMBER;
		    yylval.ival = (int)v;
		    dateflag = 2;
		} else if (*p == 'e' || *p == 'E') {
		    while (isdigit((int)*++p)) /* */;
		    if (isalpha((int)*p) || *p == '_') {
			linelim = p - line;
			return (yylex());
		    } else
			ret = FNUMBER;
		} else if (isalpha((int)*p) || *p == '_') {
		    linelim = p - line;
		    return (yylex());
		}
	    }
	    if ((!dateflag && *p=='.') || ret == FNUMBER) {
		ret = FNUMBER;
		yylval.fval = strtod(nstart, &p);
		if (p == nstart)
		    p++;
		else if (!
#ifdef HAVE_ISFINITE
		  isfinite(
#else
		  finite(
#endif
		  yylval.fval))
		    ret = K_ERR;
		else
		    decimal = TRUE;
	    } else {
		/* A NUMBER must hold at least MAXROW and MAXCOL */
		/* This is consistent with a short row and col in struct ent */
		if (v > (double)32767 || v < (double)-32768) {
		    ret = FNUMBER;
		    yylval.fval = v;
		} else {
		    temp = (int)v;
		    if((double)temp != v) {
			ret = FNUMBER;
			yylval.fval = v;
		    } else {
			ret = NUMBER;
			yylval.ival = temp;
		    }
		}
	    }
	}
	(void) signal(SIGFPE, sig_save);
    } else if (*p=='"') {
	char *ptr;
        ptr = p+1;	/* "string" or "string\"quoted\"" */
        while (*ptr && ((*ptr != '"') || (*(ptr-1) == '\\')))
	    ptr++;
        ptr = scxmalloc(ptr-p);
	yylval.sval = ptr;
	p++;
	while (*p && ((*p != '"') ||
		(*(p-1) == '\\' && *(p+1) != '\0' && *(p+1) != '\n')))
	    *ptr++ = *p++;
	*ptr = '\0';
	if (*p)
	    p++;
	ret = STRING;
    } else if (*p=='[') {
	while (*p && *p!=']')
	    p++;
	if (*p)
	    p++;
	linelim = p-line;
	tokenst = NULL;
	return yylex();
    } else ret = *p++;
    linelim = p-line;
    if (!isfunc) isfunc = ((ret == '@') + (ret == S_GOTO) - (ret == S_SET));
    if (ret == S_GOTO) isgoto = TRUE;
    tokenst = NULL;
    return ret;
}

/*
* This is a very simpleminded test for plugins:  does the file merely exist
* in the plugin directories.  Perhaps should test for it being executable
*/

int
plugin_exists(char *name, size_t len, char *path)
{
#ifndef MSDOS
    struct stat sb;
    static char *homedir;

    if ((homedir = getenv("HOME"))) {
	if (strlcpy(path, homedir, len) >= len
          || strlcat(path, "/.sc/plugins/", len) >= len
          || strlcat(path, name, len) >= len)
	    return 0;
        if (!stat(path, &sb))
	    return 1;
    }
    if (strlcpy(path, LIBDIR, len) >= len
      || strlcat(path, "/plugins/", len) >= len
      || strlcat(path, name, len) >= len)
	return 0;
    if (!stat(path, &sb))
	return 1;
#endif
    return 0;
}

/*
 * Given a token string starting with a symbolic column name and its valid
 * length, convert column name ("A"-"Z" or "AA"-"ZZ") to a column number (0-N).
 * Never mind if the column number is illegal (too high).  The procedure's name
 * and function are the inverse of coltoa().
 * 
 * Case-insensitivity is done crudely, by ignoring the 040 bit.
 */

int
atocol(char *string, int len)
{
    register int col;

    col = (toupper((int)string[0])) - 'A';

    if (len == 2)		/* has second char */
	col = ((col + 1) * 26) + ((toupper((int)string[1])) - 'A');

    return (col);
}


#ifdef SIMPLE

void
initkbd(void)
{}

void
kbd_again(void)
{}

void
resetkbd(void)
{}

# ifndef VMS

int
nmgetch(void)
{
    return (getchar());
}

# else /* VMS */

int
nmgetch(void)
/*
   This is not perfect, it doesn't move the cursor when goraw changes
   over to deraw, but it works well enough since the whole sc package
   is incredibly stable (loop constantly positions cursor).

   Question, why didn't the VMS people just implement cbreak?

   NOTE: During testing it was discovered that the DEBUGGER and curses
   and this method of reading would collide (the screen was not updated
   when continuing from screen mode in the debugger).
*/
{
    short c;
    static int key_id=0;
    int status;
#  define VMScheck(a) {if (~(status = (a)) & 1) VMS_MSG (status);}

    if (VMS_read_raw) {
      VMScheck(smg$read_keystroke (&stdkb->_id, &c, 0, 0, 0));
    } else
       c = getchar();

    switch (c) {
	case SMG$K_TRM_LEFT:  c = KEY_LEFT;  break;
	case SMG$K_TRM_RIGHT: c = KEY_RIGHT; break;
	case SMG$K_TRM_UP:    c = ctl('p');  break;
	case SMG$K_TRM_DOWN:  c = ctl('n');  break;
	default:   c = c & A_CHARTEXT;
    }
    return (c);
}


VMS_MSG (int status)
/*
   Routine to put out the VMS operating system error (if one occurs).
*/
{
#  include <descrip.h>
   char errstr[81], buf[120];
   $DESCRIPTOR(errdesc, errstr);
   short length;
#  define err_out(msg) fprintf (stderr,msg)

/* Check for no error or standard error */

    if (~status & 1) {
	status = status & 0x8000 ? status & 0xFFFFFFF : status & 0xFFFF;
	if (SYS$GETMSG(status, &length, &errdesc, 1, 0) == SS$_NORMAL) {
	    errstr[length] = '\0';
	    snprintf(buf, sizeof buf, "<0x%x> %s", status,
	        errdesc.dsc$a_pointer);
	    err_out(buf);
	} else
	    err_out("System error");
    }
}
# endif /* VMS */

#else /*SIMPLE*/

# if defined(BSD42) || defined (SYSIII) || defined(BSD43)

#  define N_KEY 4

struct key_map {
    char *k_str;
    int k_val;
    char k_index;
}; 

struct key_map km[N_KEY];

char keyarea[N_KEY*30];

char *ks;
char ks_buf[20];
char *ke;
char ke_buf[20];

#  ifdef TIOCSLTC
struct ltchars old_chars, new_chars;
#  endif

char dont_use[] = {
    ctl('['), ctl('a'), ctl('b'), ctl('c'), ctl('e'), ctl('f'), ctl('g'),
    ctl('h'), ctl('i'), ctl('j'),  ctl('l'), ctl('m'), ctl('n'), ctl('p'),
    ctl('q'), ctl('r'), ctl('s'), ctl('t'), ctl('u'), ctl('v'),  ctl('w'),
    ctl('x'), ctl('z'), 0
};

int
charout(int c) {
    return putchar(c);
}

void
initkbd(void)
{
    register struct key_map *kp;
    register i,j;
    char *p = keyarea;
    char *ktmp;
    static char buf[1024]; /* Why do I have to do this again? */

    if (!(ktmp = getenv("TERM"))) {
	(void) fprintf(stderr, "TERM environment variable not set\n");
	exit (1);
    }
    if (tgetent(buf, ktmp) <= 0)
	return;

    km[0].k_str = tgetstr("kl", &p); km[0].k_val = KEY_LEFT;
    km[1].k_str = tgetstr("kr", &p); km[1].k_val = KEY_RIGHT;
    km[2].k_str = tgetstr("ku", &p); km[2].k_val = ctl('p');
    km[3].k_str = tgetstr("kd", &p); km[3].k_val = ctl('n');

    ktmp = tgetstr("ks",&p);
    if (ktmp)  {
	strlcpy(ks_buf, ktmp, sizeof ks_buf);
	ks = ks_buf;
	tputs(ks, 1, charout);
    }
    ktmp = tgetstr("ke",&p);
    if (ktmp)  {
	strlcpy(ke_buf, ktmp, sizeof ke_buf);
	ke = ke_buf;
    }

    /* Unmap arrow keys which conflict with our ctl keys   */
    /* Ignore unset, longer than length 1, and 1-1 mapped keys */

    for (i = 0; i < N_KEY; i++) {
	kp = &km[i];
	if (kp->k_str && (kp->k_str[1] == 0) && (kp->k_str[0] != kp->k_val))
	    for (j = 0; dont_use[j] != 0; j++)
	        if (kp->k_str[0] == dont_use[j]) {
		     kp->k_str = (char *)0;
		     break;
		}
    }


#  ifdef TIOCSLTC
    (void)ioctl(fileno(stdin), TIOCGLTC, (char *)&old_chars);
    new_chars = old_chars;
    if (old_chars.t_lnextc == ctl('v'))
	new_chars.t_lnextc = -1;
    if (old_chars.t_rprntc == ctl('r'))
	new_chars.t_rprntc = -1;
    (void)ioctl(fileno(stdin), TIOCSLTC, (char *)&new_chars);
#  endif
}

void
kbd_again(void)
{
    if (ks) 
	tputs(ks, 1, charout);

#  ifdef TIOCSLTC
    (void)ioctl(fileno(stdin), TIOCSLTC, (char *)&new_chars);
#  endif
}

void
resetkbd(void)
{
    if (ke) 
	tputs(ke, 1, charout);

#  ifdef TIOCSLTC
    (void)ioctl(fileno(stdin), TIOCSLTC, (char *)&old_chars);
#  endif
}

int
nmgetch(void) {
    register int c;
    register struct key_map *kp;
    register struct key_map *biggest;
    register int i;
    int almost;
    int maybe;

    static char dumpbuf[10];
    static char *dumpindex;

#  ifdef SIGVOID
    void time_out(int);
#  else
    int time_out(int);
#  endif

    if (dumpindex && *dumpindex)
	return (*dumpindex++);

    c = getchar();
    biggest = 0;
    almost = 0;

    for (kp = &km[0]; kp < &km[N_KEY]; kp++) {
	if (!kp->k_str)
	    continue;
	if (c == kp->k_str[kp->k_index]) {
	    almost = 1;
	    kp->k_index++;
	    if (kp->k_str[kp->k_index] == 0) {
		c = kp->k_val;
		for (kp = &km[0]; kp < &km[N_KEY]; kp++)
		    kp->k_index = 0;
		return (c);
	    }
	}
	if (!biggest && kp->k_index)
	    biggest = kp;
        else if (kp->k_index && biggest->k_index < kp->k_index)
	    biggest = kp;
    }

    if (almost) { 
        (void) signal(SIGALRM, time_out);
        (void) alarm(1);

	if (setjmp(wakeup) == 0) { 
	    maybe = nmgetch();
	    (void) alarm(0);
	    return (maybe);
	}
    }
    
    if (biggest) {
	for (i = 0; i<biggest->k_index; i++) 
	    dumpbuf[i] = biggest->k_str[i];
	if (!almost)
	    dumpbuf[i++] = c;
	dumpbuf[i] = '\0';
	dumpindex = &dumpbuf[1];
	for (kp = &km[0]; kp < &km[N_KEY]; kp++)
	    kp->k_index = 0;
	return (dumpbuf[0]);
    }

    return(c);
}

# endif /* if defined(BSD42) || defined (SYSIII) || defined(BSD43) */

void
initkbd(void)
{
    keypad(stdscr, TRUE);
#ifndef NONOTIMEOUT
    notimeout(stdscr,TRUE);
#endif
}

void
kbd_again(void)
{
    keypad(stdscr, TRUE);
#ifndef NONOTIMEOUT
    notimeout(stdscr,TRUE);
#endif
}

void
resetkbd(void)
{
    keypad(stdscr, FALSE);
#ifndef NONOTIMEOUT
    notimeout(stdscr, FALSE);
#endif
}

int
nmgetch(void) {
    register int c;

    c = getch();
    switch (c) {
# ifdef KEY_SELECT
	case KEY_SELECT:	c = 'm';	break;
# endif
# ifdef KEY_C1
/* This stuff works for a wyse wy75 in ANSI mode under 5.3.  Good luck. */
/* It is supposed to map the curses keypad back to the numeric equiv. */

/* I had to disable this to make programmable function keys work.  I'm
 * not familiar with the wyse wy75 terminal.  Does anyone know how to
 * make this work without causing problems with programmable function
 * keys on everything else?  - CRM

	case KEY_C1:	c = '0'; break;
	case KEY_A1:	c = '1'; break;
	case KEY_B2:	c = '2'; break;
	case KEY_A3:	c = '3'; break;
	case KEY_F(5):	c = '4'; break;
	case KEY_F(6):	c = '5'; break;
	case KEY_F(7):	c = '6'; break;
	case KEY_F(9):	c = '7'; break;
	case KEY_F(10):	c = '8'; break;
	case KEY_F0:	c = '9'; break;
	case KEY_C3:	c = '.'; break;
	case KEY_ENTER:	c = ctl('m'); break;

 *
 *
 */
# endif
	default:	break;
    }
    return (c);
}

#endif /* SIMPLE */

#ifdef SIGVOID
void
#else
int
#endif
time_out(int signo)
{
    (void)signo;
    longjmp(wakeup, 1);
}
