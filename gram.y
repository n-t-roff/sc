/*	SC	A Spreadsheet Calculator
 *		Command and expression parser
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 * 		more mods Robert Bond 12/86
 *
 *		More mods by Alan Silverstein, 3/88, see list of changes.
 *
 *		$Revision: 7.16 $
 */



%{
#include <stdlib.h>
#include <limits.h>

#ifdef USELOCALE
#include <locale.h>
#endif

#ifndef MSDOS
#include <unistd.h>
#endif

#include "compat.h"
#include "sc.h"

#define ENULL (struct enode *)0
%}

%union {
    int ival;
    double fval;
    struct ent_ptr ent;
    struct enode *enode;
    char *sval;
    struct range_s rval;
}

%type <ent> var
%type <fval> num
%type <rval> range
%type <rval> var_or_range
%type <sval> strarg
%type <enode> e term expr_list
%token <sval> STRING
%token <ival> NUMBER
%token <fval> FNUMBER
%token <rval> RANGE
%token <rval> VAR
%token <sval> WORD
%token <sval> PLUGIN
%token <ival> COL

/*
 *  When adding new commands, make sure that any commands that may take
 *  COL as an argument precede S_FORMAT in the %token list.  All other
 *  commands must come after S_FORMAT.  This is necessary so that range
 *  names can be less than three letters without being parsed as column
 *  names.
 */

%token S_SHOW
%token S_HIDE
%token S_INSERTCOL
%token S_OPENCOL
%token S_DELETECOL
%token S_YANKCOL
%token S_GETFORMAT
%token S_FORMAT

%token S_FMT
%token S_LET
%token S_LABEL
%token S_LEFTSTRING
%token S_RIGHTSTRING
%token S_LEFTJUSTIFY
%token S_RIGHTJUSTIFY
%token S_CENTER
%token S_COLOR
%token S_ADDNOTE
%token S_DELNOTE
%token S_GET
%token S_PUT
%token S_MERGE
%token S_WRITE
%token S_TBL
%token S_COPY
%token S_MOVE
%token S_ERASE
%token S_YANK
%token S_FILL
%token S_SORT
%token S_LOCK
%token S_UNLOCK
%token S_GOTO
%token S_DEFINE
%token S_UNDEFINE
%token S_ABBREV
%token S_UNABBREV
%token S_FRAME
%token S_FRAMETOP
%token S_FRAMEBOTTOM
%token S_FRAMELEFT
%token S_FRAMERIGHT
%token S_UNFRAME
%token S_VALUE
%token S_MDIR
%token S_AUTORUN
%token S_FKEY
%token S_HISTFILE
%token S_SCEXT
%token S_ASCEXT
%token S_TBL0EXT
%token S_TBLEXT
%token S_LATEXEXT
%token S_SLATEXEXT
%token S_TEXEXT
%token S_SET
%token S_UP
%token S_DOWN
%token S_LEFT
%token S_RIGHT
%token S_ENDUP
%token S_ENDDOWN
%token S_ENDLEFT
%token S_ENDRIGHT
%token S_SELECT
%token S_INSERTROW
%token S_OPENROW
%token S_DELETEROW
%token S_YANKROW
%token S_PULL
%token S_PULLMERGE
%token S_PULLROWS
%token S_PULLCOLS
%token S_PULLXCHG
%token S_PULLTP
%token S_PULLFMT
%token S_PULLCOPY
%token S_WHEREAMI
%token S_GETNUM
%token S_FGETNUM
%token S_GETSTRING
%token S_GETEXP
%token S_GETFMT
%token S_GETFRAME
%token S_GETRANGE
%token S_EVAL
%token S_SEVAL
%token S_QUERY
%token S_GETKEY
%token S_ERROR
%token S_RECALC
%token S_REDRAW
%token S_QUIT
%token S_STATUS
%token S_RUN
%token S_PLUGIN
%token S_PLUGOUT

%token K_ERROR
%token K_INVALID
%token K_FIXED
%token K_SUM
%token K_PROD
%token K_AVG
%token K_STDDEV
%token K_COUNT
%token K_ROWS
%token K_COLS
%token K_ABS
%token K_ACOS
%token K_ASIN
%token K_ATAN
%token K_ATAN2
%token K_CEIL
%token K_COS
%token K_EXP
%token K_FABS
%token K_FLOOR
%token K_HYPOT
%token K_LN
%token K_LOG
%token K_PI
%token K_POW
%token K_SIN
%token K_SQRT
%token K_TAN
%token K_DTR
%token K_RTD
%token K_MAX
%token K_MIN
%token K_RND
%token K_ROUND
%token K_IF

%token K_PV
%token K_FV
%token K_PMT

%token K_HOUR
%token K_MINUTE
%token K_SECOND
%token K_MONTH
%token K_DAY
%token K_YEAR
%token K_NOW
%token K_DATE
%token K_DTS
%token K_TTS
%token K_FMT
%token K_SUBSTR
%token K_UPPER
%token K_LOWER
%token K_CAPITAL
%token K_STON
%token K_EQS
%token K_EXT
%token K_NVAL
%token K_SVAL
%token K_LOOKUP
%token K_HLOOKUP
%token K_VLOOKUP
%token K_INDEX
%token K_STINDEX
%token K_AUTO
%token K_AUTOCALC
%token K_AUTOINSERT
%token K_AUTOWRAP
%token K_CSLOP
%token K_BYROWS
%token K_BYCOLS
%token K_OPTIMIZE
%token K_ITERATIONS
%token K_NUMERIC
%token K_PRESCALE
%token K_EXTFUN
%token K_CELLCUR
%token K_TOPROW
%token K_COLOR
%token K_COLORNEG
%token K_COLORERR
%token K_BRAILLE
%token K_BACKUP
%token K_MOUSE
%token K_BLACK
%token K_RED
%token K_GREEN
%token K_YELLOW
%token K_BLUE
%token K_MAGENTA
%token K_CYAN
%token K_WHITE
%token K_TBLSTYLE
%token K_TBL
%token K_LATEX
%token K_SLATEX
%token K_TEX
%token K_FRAME
%token K_RNDTOEVEN
%token K_FILENAME
%token K_MYROW
%token K_MYCOL
%token K_LASTROW
%token K_LASTCOL
%token K_COLTOA
%token K_CRACTION
%token K_CRROW
%token K_CRCOL
%token K_ROWLIMIT
%token K_COLLIMIT
%token K_PAGESIZE
%token K_NUMITER
%token K_ERR
%token K_SCRC
%token K_LOCALE
  
%right ';'
%left '?' ':'
%left '|'
%left '&'
%nonassoc '<' '=' '>' '!'
%left '+' '-' '#'
%left '*' '/' '%'
%left '^'

%%
command:	S_LET var_or_range '=' e
				{ let($2.left.vp, $4); }
	|	S_LET var_or_range '='
				{ $2.left.vp->v = (double) 0.0;
				  if ($2.left.vp->expr &&
					!($2.left.vp->flags & IS_STREXPR)) {
				    efree($2.left.vp->expr);
				    $2.left.vp->expr = NULL;
				  }
				  $2.left.vp->cellerror = CELLOK;
				  $2.left.vp->flags &= ~IS_VALID;
				  $2.left.vp->flags |= IS_CHANGED;
				  changed++;
				  FullUpdate++;
				  modflg++; }
	|	S_LABEL var_or_range '=' e
				{ slet($2.left.vp, $4, 0); }
	|	S_LEFTSTRING var_or_range '=' e
				{ slet($2.left.vp, $4, -1); }
	|	S_RIGHTSTRING var_or_range '=' e
				{ slet($2.left.vp, $4, 1); }
	|	S_LEFTJUSTIFY var_or_range
				{ ljustify($2.left.vp->row, $2.left.vp->col,
				    $2.right.vp->row, $2.right.vp->col); }
	|	S_LEFTJUSTIFY
				{ if (showrange)
				    ljustify(showsr, showsc, currow, curcol);
				}
	|	S_RIGHTJUSTIFY var_or_range
				{ rjustify($2.left.vp->row, $2.left.vp->col,
				    $2.right.vp->row, $2.right.vp->col); }
	|	S_RIGHTJUSTIFY
				{ if (showrange)
				    rjustify(showsr, showsc, currow, curcol);
				}
	|	S_CENTER var_or_range
				{ center($2.left.vp->row, $2.left.vp->col,
				    $2.right.vp->row, $2.right.vp->col); }
	|	S_CENTER
				{ if (showrange)
				    center(showsr, showsc, currow, curcol);
				}
	|	S_ADDNOTE var	{ if (showrange) {
				    $2.vp->nrow=currow<showsr?currow:showsr;
				    $2.vp->ncol=curcol<showsc?curcol:showsc;
				    $2.vp->nlastrow=currow<showsr?showsr:currow;
				    $2.vp->nlastcol=curcol<showsc?showsc:curcol;
				  } else {
				    $2.vp->nrow=currow;
				    $2.vp->ncol=curcol;
				    $2.vp->nlastrow=currow;
				    $2.vp->nlastcol=curcol;
				  }
				  $2.vp->flags |= IS_CHANGED;
				  FullUpdate++;
				  modflg++;
				}
	|	S_ADDNOTE var var_or_range
				{ $2.vp->nrow = $3.left.vp->row;
				  $2.vp->ncol = $3.left.vp->col;
				  $2.vp->nlastrow = $3.right.vp->row;
				  $2.vp->nlastcol = $3.right.vp->col;
				  $2.vp->flags |= IS_CHANGED;
				  FullUpdate++;
				  modflg++;
				}
	|	S_DELNOTE var	{ $2.vp->nrow = $2.vp->ncol = -1; }
	|	S_DELNOTE	{ struct ent *p;
				  p = lookat(currow, curcol);
				  p->nrow = p->ncol = -1;
				  p->flags |= IS_CHANGED;
				  modflg++;
				}
	|	S_FORMAT COL ':' COL NUMBER NUMBER NUMBER
				{ doformat($2,$4,$5,$6,$7); }
	|	S_FORMAT COL NUMBER NUMBER NUMBER
				{ doformat($2,$2,$3,$4,$5); }
        |       S_FORMAT COL ':' COL NUMBER NUMBER
                                { doformat($2,$4,$5,$6, REFMTFIX); }
        |       S_FORMAT COL NUMBER NUMBER
                                { doformat($2,$2,$3,$4, REFMTFIX); }
        |       S_FORMAT NUMBER '=' STRING
				{ if ($2 >= 0 && $2 < 10) {
				    if (colformat[$2])
					scxfree(colformat[$2]);
				    if (strlen($4))
					colformat[$2] = $4;
				    else
					colformat[$2] = NULL;
				    FullUpdate++;
				    modflg++;
				  } else
				    error("Invalid format number");
				}
	|	S_GET strarg	{  /* This tmp hack is because readfile
				    * recurses back through yyparse. */
				    char *tmp;
				    tmp = $2;
				    readfile(tmp, 1);
				    scxfree(tmp);
				}
	|	S_MERGE strarg	{
				    char *tmp;
				    tmp = $2;
				    readfile(tmp, 0);
				    scxfree(tmp);
				}
	|	S_MDIR strarg	{ if (mdir) scxfree(mdir);
				  if (strlen($2))
				    mdir = $2;
				  modflg++; }
	|	S_AUTORUN strarg
				{ if (autorun) scxfree(autorun);
				  if (strlen($2))
				    autorun = $2;
				  modflg++; }
	|	S_FKEY NUMBER '=' strarg
				{ if ($2 > 0 && $2 <= FKEYS) {
				    if (fkey[$2 - 1]) {
					scxfree(fkey[$2 - 1]);
					fkey[$2 - 1] = NULL;
				    }
				    if (strlen($4))
					fkey[$2 - 1] = $4;
				    modflg++;
				  } else
				    error("Invalid function key");
				}
	|	S_SCEXT strarg	{ if (scext) scxfree(scext); scext = $2; }
	|	S_HISTFILE strarg { strlcpy(histfile, $2, sizeof histfile);
				    scxfree($2); }
	|	S_ASCEXT strarg	{ if (ascext) scxfree(ascext); ascext = $2; }
	|	S_TBL0EXT strarg
				{ if (tbl0ext) scxfree(tbl0ext); tbl0ext = $2; }
	|	S_TBLEXT strarg	{ if (tblext) scxfree(tblext); tblext = $2; }
	|	S_LATEXEXT strarg	{ if (latexext) scxfree(latexext);
					    latexext = $2; }
	|	S_SLATEXEXT strarg	{ if (slatexext) scxfree(slatexext);
					    slatexext = $2; }
	|	S_TEXEXT strarg	{ if (texext) scxfree(texext); texext = $2; }
	|       S_PUT strarg range
				{ (void) writefile($2, ($3.left.vp)->row, 
					($3.left.vp)->col, ($3.right.vp)->row,
					($3.right.vp)->col);
					    scxfree($2); }
	|	S_PUT strarg	{ (void) writefile($2, 0, 0, maxrow, maxcol);
					    scxfree($2); }
	|       S_PUT range		{ (void) write_cells(stdout,
					  $2.left.vp->row, $2.left.vp->col,
					  $2.right.vp->row, $2.right.vp->col,
					  $2.left.vp->row, $2.left.vp->col); }
	|       S_PUT range '/' var_or_range
					{ (void) write_cells(stdout,
					  $2.left.vp->row, $2.left.vp->col,
					  $2.right.vp->row, $2.right.vp->col,
					  $4.left.vp->row, $4.left.vp->col); }
	|       S_PUT '%' '/' var_or_range
					{ (void) write_cells(stdout, 0, 0,
					  maxrow, maxcol,
					  $4.left.vp->row, $4.left.vp->col); }
	|       S_PUT '/' var_or_range	{ (void) write_cells(stdout,
					  showsr, showsc, currow, curcol,
					  $3.left.vp->row, $3.left.vp->col); }
	|	S_PUT '%'	{ (void) write_cells(stdout, 0, 0,
					    maxrow, maxcol, 0, 0); }
	|	S_PUT		{ (void) write_cells(stdout, 0, 0,
					    maxrow, maxcol, 0, 0); }
	|       S_WRITE strarg range { (void) printfile($2, ($3.left.vp)->row, 
					($3.left.vp)->col, ($3.right.vp)->row,
					($3.right.vp)->col);
					    scxfree($2); }
	|	S_WRITE strarg	{ (void) printfile($2, 0, 0, maxrow, maxcol);
					    scxfree($2); }
	|       S_WRITE range		{ (void) printfile(NULL,
					  $2.left.vp->row, $2.left.vp->col,
					  $2.right.vp->row, $2.right.vp->col); }
	|	S_WRITE '%'	{ (void) printfile(NULL, 0, 0,
					    maxrow, maxcol); }
	|	S_WRITE		{ (void) printfile(NULL, 0, 0,
					    maxrow, maxcol); }
	|       S_TBL strarg range { (void) tblprintfile($2, ($3.left.vp)->row, 
					($3.left.vp)->col, ($3.right.vp)->row,
					($3.right.vp)->col);
					    scxfree($2); }
	|	S_TBL strarg	{ (void)tblprintfile($2, 0, 0, maxrow, maxcol);
					    scxfree($2); }
	|       S_SHOW COL ':' COL	{ showcol($2, $4); }
	|       S_SHOW NUMBER ':' NUMBER
					{ showrow($2, $4); }
 	|	S_HIDE			{ int a;
	    				  if (showrange == SHOWROWS) {
					    if (showsr < currow) {
						int r = currow;
						currow = showsr;
						showsr = r;
					    }
					    a = showsr - currow + 1;
					    hiderow(a);
					  } else if (showrange == SHOWCOLS) {
					    if (showsc < curcol) {
						int c = curcol;
						curcol = showsc;
						showsc = c;
					    }
					    a = showsc - curcol + 1;
					    hidecol(a);
					  } else
					    a = 1;
					}
 	|	S_HIDE COL		{ hide_col($2); }
	|	S_HIDE COL ':' COL	{ int c = curcol, a;
					  if ($2 < $4) {
					    curcol = $2;
					    a = $4 - $2 + 1;
					  } else {
					      curcol = $4;
					      a = $2 - $4 + 1;
					  }
					  hidecol(a);
					  curcol = c < curcol ? c :
					      c < curcol + a ? curcol :
					      c - a;
					}
 	|	S_HIDE NUMBER		{ hide_row($2); }
	|	S_HIDE NUMBER ':' NUMBER
					{ int r = currow, a;
					  if ($2 < $4) {
					    currow = $2;
					    a = $4 - $2 + 1;
					  } else {
					      currow = $4;
					      a = $2 - $4 + 1;
					  }
					  hiderow(a);
					  currow = r < currow ? r :
					      r < currow + a ? currow :
					      r - a;
					}
	|	S_COPY			{ if (showrange) {
					    showrange = 0;
					    copy(lookat(showsr, showsc),
					    lookat(currow, curcol),
					    NULL, NULL);
					  } else
					    copy(lookat(currow, curcol),
					    lookat(currow, curcol),
					    NULL, NULL); }
	|	S_COPY range		{ copy($2.left.vp, $2.right.vp,
					    NULL, NULL); }
	|	S_COPY range var_or_range 
					{ copy($2.left.vp, $2.right.vp,
					    $3.left.vp, $3.right.vp); }
	|	S_MOVE var		{ mover($2.vp, lookat(showsr, showsc),
					    lookat(currow, curcol)); }
	|	S_MOVE var var_or_range	{ mover($2.vp, $3.left.vp,
					    $3.right.vp); }
	|	S_ERASE			{ eraser(lookat(showsr, showsc),
					    lookat(currow, curcol)); }
	|	S_ERASE var_or_range	{ eraser($2.left.vp, $2.right.vp); }
	|	S_YANK			{ yankr(lookat(showsr, showsc),
					    lookat(currow, curcol)); }
	|	S_YANK var_or_range	{ yankr($2.left.vp, $2.right.vp); }
	|	S_VALUE       { valueize_area(showsr, showsc, currow, curcol);
					    modflg++; }
	|	S_VALUE var_or_range	{ valueize_area(($2.left.vp)->row,
					    ($2.left.vp)->col,
					    ($2.right.vp)->row,
					    ($2.right.vp)->col); modflg++; }
	|	S_FILL var_or_range num num
				{ fill($2.left.vp, $2.right.vp, $3, $4); }
	|	S_SORT		{ sortrange(lookat(showsr, showsc),
				  lookat(currow, curcol), NULL); }
	|	S_SORT range	{ sortrange($2.left.vp, $2.right.vp, NULL); }
	|	S_SORT range strarg
				{ sortrange($2.left.vp, $2.right.vp, $3); }
	|	S_FMT var_or_range STRING
				{ format_cell($2.left.vp, $2.right.vp, $3); }
	|	S_LOCK		{ lock_cells(lookat(showsr, showsc),
				    lookat(currow, curcol)); }
	|	S_LOCK var_or_range
				{ lock_cells($2.left.vp, $2.right.vp); }
	|	S_UNLOCK		{ unlock_cells(lookat(showsr, showsc),
					    lookat(currow, curcol)); }
	|	S_UNLOCK var_or_range
				{ unlock_cells($2.left.vp, $2.right.vp); }
	|	S_GOTO var_or_range var_or_range
				{ moveto($2.left.vp->row, $2.left.vp->col,
					    $2.right.vp->row, $2.right.vp->col,
					    $3.left.vp->row, $3.left.vp->col); }
	|	S_GOTO var_or_range { moveto($2.left.vp->row, $2.left.vp->col,
					    $2.right.vp->row, $2.right.vp->col,
					    -1, -1); }
	|       S_GOTO num range	{ num_search($2,
					  $3.left.vp->row, $3.left.vp->col,
					  $3.right.vp->row,
					  $3.right.vp->col, 0, 0); }
	|       S_GOTO num		{ num_search($2, 0, 0,
					  maxrow, maxcol, 0, 0); }
	|       S_GOTO errlist
	|       S_GOTO STRING range	{ str_search($2,
					  $3.left.vp->row, $3.left.vp->col,
					  $3.right.vp->row,
					  $3.right.vp->col, 0, 0); }
	|       S_GOTO '#' STRING range	{ str_search($3,
					  $4.left.vp->row, $4.left.vp->col,
					  $4.right.vp->row,
					  $4.right.vp->col, 1, 0); }
	|       S_GOTO '%' STRING range	{ str_search($3,
					  $4.left.vp->row, $4.left.vp->col,
					  $4.right.vp->row,
					  $4.right.vp->col, 2, 0); }
	|       S_GOTO STRING		{ str_search($2, 0, 0,
					  maxrow, maxcol, 0, 0); }
	|       S_GOTO '#' STRING	{ str_search($3, 0, 0,
					  maxrow, maxcol, 1, 0); }
	|       S_GOTO '%' STRING	{ str_search($3, 0, 0,
					  maxrow, maxcol, 2, 0); }
	|	S_GOTO			{ go_last(0); }
	|	S_GOTO WORD		{ /* don't repeat last goto on
						"unintelligible word" */ ; }
	|	S_DEFINE strarg		{ struct ent_ptr arg1, arg2;
					  arg1.vp = lookat(showsr, showsc);
					  arg1.vf = 0;
					  arg2.vp = lookat(currow, curcol);
					  arg2.vf = 0;
                                          if (arg1.vp == arg2.vp || !showrange)
                                             add_range($2, arg2, arg2, 0);
                                          else
                                             add_range($2, arg1, arg2, 1); }

	|	S_DEFINE strarg range	{ add_range($2, $3.left, $3.right, 1); }
	|	S_DEFINE strarg var	{ add_range($2, $3, $3, 0); }
	|	S_UNDEFINE var_or_range	{ del_range($2.left.vp, $2.right.vp); }
	|       S_ABBREV STRING		{ add_abbr($2); }
	|       S_ABBREV		{ add_abbr(NULL); }
	|       S_UNABBREV STRING	{ del_abbr($2); }
	|	S_FRAME range range	{ add_frange($2.left.vp, $2.right.vp,
						$3.left.vp, $3.right.vp,
						0, 0, 0, 0); }
	|	S_FRAME range		{ if (showrange) {
					    showrange = 0;
					    add_frange($2.left.vp, $2.right.vp,
						lookat(showsr, showsc),
						lookat(currow, curcol),
						0, 0, 0, 0);
					  } else {
					    struct frange *cfr;
					    cfr = find_frange(currow, curcol);
					    if (cfr) {
						add_frange(cfr->or_left,
						    cfr->or_right,
						    $2.left.vp, $2.right.vp,
						    0, 0, 0, 0);
					    }
					  }
					}
	|	S_FRAME			{ struct frange *cfr;
					  /* cfr points to current frange */
					  cfr = find_frange(currow, curcol);
					  if (showrange && cfr) {
					    showrange = 0;
					    add_frange(cfr->or_left,
						cfr->or_right,
						lookat(showsr, showsc),
						lookat(currow, curcol),
						0, 0, 0, 0);
					  } else {
					    error("Need both outer and inner"
						    " ranges to create frame");
					  }
					}
	|	S_FRAMETOP range NUMBER
					{ add_frange($2.left.vp, $2.right.vp,
						NULL, NULL, $3, -1, -1, -1); }
	|	S_FRAMETOP NUMBER	{ struct frange *cfr;
					  /* cfr points to current frange */
					  cfr = find_frange(currow, curcol);
					  if (cfr)
					    add_frange(cfr->or_left,
						cfr->or_right,
						NULL, NULL, $2, -1, -1, -1); }
	|	S_FRAMEBOTTOM range NUMBER
					{ add_frange($2.left.vp, $2.right.vp,
						NULL, NULL, -1, $3, -1, -1); }
	|	S_FRAMEBOTTOM NUMBER	{ struct frange *cfr;
					  /* cfr points to current frange */
					  cfr = find_frange(currow, curcol);
					  if (cfr)
					    add_frange(cfr->or_left,
						cfr->or_right,
						NULL, NULL, -1, $2, -1, -1); }
	|	S_FRAMELEFT range NUMBER
					{ add_frange($2.left.vp, $2.right.vp,
						NULL, NULL, -1, -1, $3, -1); }
	|	S_FRAMELEFT NUMBER	{ struct frange *cfr;
					  /* cfr points to current frange */
					  cfr = find_frange(currow, curcol);
					  if (cfr)
					    add_frange(cfr->or_left,
						cfr->or_right,
						NULL, NULL, -1, -1, $2, -1); }
	|	S_FRAMERIGHT range NUMBER
					{ add_frange($2.left.vp, $2.right.vp,
						NULL, NULL, -1, -1, -1, $3); }
	|	S_FRAMERIGHT NUMBER	{ struct frange *cfr;
					  /* cfr points to current frange */
					  cfr = find_frange(currow, curcol);
					  if (cfr)
					    add_frange(cfr->or_left,
						cfr->or_right,
						NULL, NULL, -1, -1, -1, $2); }
	|	S_UNFRAME range		{ add_frange($2.left.vp, $2.right.vp,
						NULL, NULL, 0, 0, 0, 0); }
	|	S_UNFRAME		{ struct frange *cfr;
					  /* cfr points to current frange */
					  cfr = find_frange(currow, curcol);
					  if (cfr)
					    add_frange(cfr->or_left,
						cfr->or_right,
						NULL, NULL, 0, 0, 0, 0); }
	|	S_COLOR NUMBER '='	{ if ($2 > 0 && $2 < 9)
					    initcolor($2);
					  else
					    error("Invalid color number"); }
	|	S_COLOR NUMBER '=' e	{ change_color($2, $4); }
	|	S_COLOR range NUMBER	{ add_crange($2.left.vp, $2.right.vp,
						$3); }
	|	S_SET setlist		{ modflg++; }
	|	S_UP			{ backrow( 1); }
	|	S_UP NUMBER		{ backrow($2); }
	|	S_DOWN			{ forwrow( 1); }
	|	S_DOWN NUMBER		{ forwrow($2); }
	|	S_LEFT			{ backcol( 1); }
	|	S_LEFT NUMBER		{ backcol($2); }
	|	S_RIGHT			{ forwcol( 1); }
	|	S_RIGHT NUMBER		{ forwcol($2); }
	|	S_ENDUP			{ doend(-1, 0); }
	|	S_ENDDOWN		{ doend( 1, 0); }
	|	S_ENDLEFT		{ doend( 0,-1); }
	|	S_ENDRIGHT		{ doend( 0, 1); }
	|	S_SELECT STRING		{ int c;
					  if ((c = *$2) >= '0' && c <= '9') {
					    qbuf = c - '0' + (DELBUFSIZE - 10);
					  } else if (c >= 'a' && c <= 'z') {
					    qbuf = c - 'a' + (DELBUFSIZE - 36);
					  } else if (c == '"') {
					    qbuf = 0;
					  } else
					    error("Invalid buffer");
					  scxfree($2);
					}
	|	S_INSERTROW		{ insertrow( 1, 0); }
	|	S_INSERTROW '*' NUMBER	{ insertrow($3, 0); }
	|	S_OPENROW		{ insertrow( 1, 1); }
	|	S_OPENROW '*' NUMBER	{ insertrow($3, 1); }
	|	S_INSERTCOL		{ insertcol( 1, 0); }
	|	S_INSERTCOL '*' NUMBER	{ insertcol($3, 0); }
	|	S_OPENCOL		{ insertcol( 1, 1); }
	|	S_OPENCOL '*' NUMBER	{ insertcol($3, 1); }
	|	S_DELETEROW		{ int a;
	    				  if (showrange == SHOWROWS) {
					    if (showsr < currow) {
						int r = currow;
						currow = showsr;
						showsr = r;
					    }
					    a = showsr - currow + 1;
					  } else
					    a = 1;
					  deleterow(a);
					}
	|	S_DELETEROW '*' NUMBER	{ deleterow($3); }
	|	S_DELETEROW NUMBER	{ int r = currow;
					  currow = $2;
					  deleterow(1);
					  currow = r <= currow ? r : r - 1;
					}
	|	S_DELETEROW NUMBER ':' NUMBER
					{ int r = currow, a;
					  if ($2 < $4) {
					    currow = $2;
					    a = $4 - $2 + 1;
					  } else {
					      currow = $4;
					      a = $2 - $4 + 1;
					  }
					  deleterow(a);
					  currow = r < currow ? r :
					      r < currow + a ? currow :
					      r - a;
					}
	|	S_DELETECOL		{ int a;
	    				  if (showrange == SHOWCOLS) {
					    if (showsc < curcol) {
						int c = curcol;
						curcol = showsc;
						showsc = c;
					    }
					    a = showsc - curcol + 1;
					  } else
					    a = 1;
					  closecol(a);
					}
	|	S_DELETECOL COL		{ int r = curcol;
					  curcol = $2;
					  closecol(1);
					  curcol = r <= curcol ? r : r - 1;
					}
	|	S_DELETECOL '*' NUMBER	{ closecol($3); }
	|	S_DELETECOL COL ':' COL	{ int c = curcol, a;
					  if ($2 < $4) {
					    curcol = $2;
					    a = $4 - $2 + 1;
					  } else {
					      curcol = $4;
					      a = $2 - $4 + 1;
					  }
					  closecol(a);
					  curcol = c < curcol ? c :
					      c < curcol + a ? curcol :
					      c - a;
					}
	|	S_YANKROW		{ int r = currow, a;
	    				  if (showrange == SHOWROWS) {
					    if (showsr < currow) {
						currow = showsr;
						showsr = r;
					    }
					    a = showsr - currow + 1;
					  } else
					    a = 1;
					  yankrow(a);
					  currow = r;
					}
	|	S_YANKROW '*' NUMBER	{ yankrow($3); }
	|	S_YANKROW NUMBER	{ int r = currow;
					  currow = $2;
					  yankrow(1);
					  currow = r;
					}
	|	S_YANKROW NUMBER ':' NUMBER
					{ int r = currow, a;
					  if ($2 < $4) {
					    currow = $2;
					    a = $4 - $2 + 1;
					  } else {
					      currow = $4;
					      a = $2 - $4 + 1;
					  }
					  yankrow(a);
					  currow = r;
					}
	|	S_YANKCOL		{ int c = curcol, a;
	    				  if (showrange == SHOWCOLS) {
					    if (showsc < curcol) {
						curcol = showsc;
						showsc = c;
					    }
					    a = showsc - curcol + 1;
					  } else
					    a = 1;
					  yankcol(a);
					  curcol = c;
					}
	|	S_YANKCOL NUMBER	{ int c = curcol;
					  curcol = $2;
					  yankcol(1);
					  curcol = c;
					}
	|	S_YANKCOL '*' NUMBER	{ yankcol($3); }
	|	S_YANKCOL COL ':' COL	{ int c = curcol, a;
					  if ($2 < $4) {
					    curcol = $2;
					    a = $4 - $2 + 1;
					  } else {
					      curcol = $4;
					      a = $2 - $4 + 1;
					  }
					  yankcol(a);
					  curcol = c;
					}
	|	S_PULL			{ pullcells('p'); }
	|	S_PULLMERGE		{ pullcells('m'); }
	|	S_PULLROWS		{ pullcells('r'); }
	|	S_PULLCOLS		{ pullcells('c'); }
	|	S_PULLXCHG		{ pullcells('x'); }
	|	S_PULLTP		{ pullcells('t'); }
	|	S_PULLFMT		{ pullcells('f'); }
	|	S_PULLCOPY		{ copy(NULL, NULL, NULL, NULL); }
	|	S_PULLCOPY var_or_range	{ copy($2.left.vp, $2.right.vp,
					     NULL, (struct ent *)1); }
	|	S_WHEREAMI		{ size_t l;
					  snprintf(line, sizeof line, "%s%d ",
					     coltoa(curcol), currow);
					  l = strlen(line);
					  snprintf(line + l, sizeof(line) - l,
					     "%s%d\n", coltoa(stcol), strow);
					  write(macrofd, line, strlen(line));
					  line[0] = '\0'; }
	|	S_WHEREAMI '|' NUMBER	{ size_t l;
					  snprintf(line, sizeof line, "%s%d ",
					     coltoa(curcol), currow);
					  l = strlen(line);
					  snprintf(line + l, sizeof(line) - l,
					     "%s%d\n", coltoa(stcol), strow);
					  write($3, line, strlen(line));
					  line[0] = '\0'; }
	|	S_GETNUM var_or_range	{ getnum($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, macrofd); }
	|	S_GETNUM var_or_range '|' NUMBER
					{ getnum($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, $4); }
	|	S_GETNUM		{ getnum(currow, curcol,
						currow, curcol, macrofd); }
	|	S_GETNUM '|' NUMBER	{ getnum(currow, curcol,
						currow, curcol, $3); }
	|	S_FGETNUM var_or_range	{ fgetnum($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, macrofd); }
	|	S_FGETNUM var_or_range '|' NUMBER
					{ fgetnum($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, $4); }
	|	S_FGETNUM		{ fgetnum(currow, curcol,
						currow, curcol, macrofd); }
	|	S_FGETNUM '|' NUMBER	{ fgetnum(currow, curcol,
						currow, curcol, $3); }
	|	S_GETSTRING var_or_range
					{ getstring($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, macrofd); }
	|	S_GETSTRING var_or_range '|' NUMBER
					{ getstring($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, $4); }
	|	S_GETSTRING		{ getstring(currow, curcol,
						currow, curcol, macrofd); }
	|	S_GETSTRING '|' NUMBER	{ getstring(currow, curcol,
						currow, curcol, $3); }
	|	S_GETEXP var_or_range	{ getexp($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, macrofd); }
	|	S_GETEXP var_or_range '|' NUMBER
					{ getexp($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, $4); }
	|	S_GETEXP		{ getexp(currow, curcol,
						currow, curcol, macrofd); }
	|	S_GETEXP '|' NUMBER	{ getexp(currow, curcol,
						currow, curcol, $3); }
	|	S_GETFORMAT COL		{ getformat($2, macrofd); }
	|	S_GETFORMAT COL '|' NUMBER
					{ getformat($2, $4); }
	|	S_GETFORMAT		{ getformat(curcol, macrofd); }
	|	S_GETFORMAT '|' NUMBER	{ getformat(curcol, $3); }
	|	S_GETFMT var_or_range	{ getfmt($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, macrofd); }
	|	S_GETFMT var_or_range '|' NUMBER
					{ getfmt($2.left.vp->row,
						$2.left.vp->col,
						$2.right.vp->row,
						$2.right.vp->col, $4); }
	|	S_GETFMT		{ getfmt(currow, curcol,
						currow, curcol, macrofd); }
	|	S_GETFMT '|' NUMBER	{ getfmt(currow, curcol,
						currow, curcol, $3); }
	|	S_GETFRAME		{ getframe(macrofd); }
	|	S_GETFRAME '|' NUMBER	{ getframe($3); }
	|	S_GETRANGE STRING	{ getrange($2, macrofd); }
	|	S_GETRANGE STRING '|' NUMBER
					{ getrange($2, $4); }
	|	S_EVAL e		{ doeval($2, NULL, currow, curcol,
						macrofd); }
	|	S_EVAL e STRING		{ doeval($2, $3, currow, curcol,
						macrofd); }
	|	S_EVAL e STRING '|' NUMBER
					{ doeval($2, $3, currow, curcol,
						$5); }
	|	S_SEVAL e		{ doseval($2, currow, curcol, macrofd); }
	|	S_QUERY STRING STRING	{ doquery($2, $3, macrofd); }
	|	S_QUERY STRING STRING '|' NUMBER
					{ doquery($2, $3, $5); }
	|	S_QUERY STRING		{ doquery($2, NULL, macrofd); }
	|	S_QUERY STRING '|' NUMBER
					{ doquery($2, NULL, $4); }
	|	S_QUERY			{ doquery(NULL, NULL, macrofd); }
	|	S_QUERY '|' NUMBER	{ doquery(NULL, NULL, $3); }
	|	S_GETKEY		{ dogetkey(); }
	|	S_ERROR STRING		{ error("%s", $2); }
	|	S_STATUS			{ dostat(macrofd); }
	|	S_STATUS '|' NUMBER	{ dostat($3); }
	|	S_RECALC		{ EvalAll();
					  update(1);
					  changed = 0;
					}
	|	S_REDRAW		{ if (usecurses) {
					    clearok(stdscr, TRUE);
					    linelim = -1;
					    update(1);
					    refresh();
					    changed = 0;
					  }
					}
	|	S_QUIT			{ stopdisp(); exit(0); }
	|	S_RUN STRING		{ deraw(1);
					  system($2);
					  if (*($2 + strlen($2) - 1) != '&') {
					    printf("Press any key to continue ");
					    fflush(stdout);
					    cbreak();
					    nmgetch();
					  }
					  goraw();
					  scxfree($2); }
	|	S_PLUGIN STRING '=' STRING
					{ addplugin($2, $4, 'r'); } 
	|	S_PLUGOUT STRING '=' STRING
					{ addplugin($2, $4, 'w'); } 
	|       PLUGIN			{ *line = '|';
					  snprintf(line + 1, sizeof(line) - 1,
					      "%s", $1);
					  readfile(line, 0);
					  scxfree($1); }
	|	/* nothing */
	|	error;

term: 		var			{ $$ = new_var(O_VAR, $1); }
	|	'@' K_FIXED term	{ $$ = new('f', $3, ENULL); }
	|	'(' '@' K_FIXED ')' term
					{ $$ = new('F', $5, ENULL); }
	|       '@' K_SUM '(' var_or_range ')' 
				{ $$ = new(SUM,
					new_range(REDUCE | SUM, $4), ENULL); }
	|	'@' K_SUM  '(' range ',' e ')'
				{ $$ = new(SUM,
					new_range(REDUCE | SUM, $4), $6); }
	|       '@' K_PROD '(' var_or_range ')' 
				{ $$ = new(PROD,
					new_range(REDUCE | PROD, $4), ENULL); }
	|	'@' K_PROD  '(' range ',' e ')'
				{ $$ = new(PROD,
					new_range(REDUCE | PROD, $4), $6); }
	|       '@' K_AVG '(' var_or_range ')' 
				{ $$ = new(AVG,
					new_range(REDUCE | AVG, $4), ENULL); }
	|	'@' K_AVG  '(' range ',' e ')'
				{ $$ = new(AVG,
					new_range(REDUCE | AVG, $4), $6); }
	|       '@' K_STDDEV '(' var_or_range ')' 
				{ $$ = new(STDDEV,
					new_range(REDUCE | STDDEV, $4), ENULL); }
	|	'@' K_STDDEV  '(' range ',' e ')'
				{ $$ = new(STDDEV,
					new_range(REDUCE | STDDEV, $4), $6); }
	|       '@' K_COUNT '(' var_or_range ')' 
				{ $$ = new(COUNT,
					new_range(REDUCE | COUNT, $4), ENULL); }
	|	'@' K_COUNT  '(' range ',' e ')'
				{ $$ = new(COUNT,
					new_range(REDUCE | COUNT, $4), $6); }
	|       '@' K_MAX '(' var_or_range ')' 
				{ $$ = new(MAX,
					new_range(REDUCE | MAX, $4), ENULL); }
	|	'@' K_MAX  '(' range ',' e ')'
				{ $$ = new(MAX,
					new_range(REDUCE | MAX, $4), $6); }
	|	'@' K_MAX '(' e ',' expr_list ')'
				{ $$ = new(LMAX, $6, $4); }
	|       '@' K_MIN '(' var_or_range ')' 
				{ $$ = new(MIN,
					new_range(REDUCE | MIN, $4), ENULL); }
	|	'@' K_MIN  '(' range ',' e ')'
				{ $$ = new(MIN,
					new_range(REDUCE | MIN, $4), $6); }
	|	'@' K_MIN '(' e ',' expr_list ')'
				{ $$ = new(LMIN, $6, $4); }
	|       '@' K_ROWS '(' var_or_range ')' 
				{ $$ = new_range(REDUCE | 'R', $4); }
	|       '@' K_COLS '(' var_or_range ')' 
				{ $$ = new_range(REDUCE | 'C', $4); }
	| '@' K_ABS '(' e ')'		{ $$ = new(ABS, $4, ENULL); }
	| '@' K_ACOS '(' e ')'		{ $$ = new(ACOS, $4, ENULL); }
	| '@' K_ASIN '(' e ')'		{ $$ = new(ASIN, $4, ENULL); }
	| '@' K_ATAN '(' e ')'		{ $$ = new(ATAN, $4, ENULL); }
	| '@' K_ATAN2 '(' e ',' e ')'	{ $$ = new(ATAN2, $4, $6); }
	| '@' K_CEIL '(' e ')'		{ $$ = new(CEIL, $4, ENULL); }
	| '@' K_COS '(' e ')'		{ $$ = new(COS, $4, ENULL); }
	| '@' K_EXP '(' e ')'		{ $$ = new(EXP, $4, ENULL); }
	| '@' K_FABS '(' e ')'		{ $$ = new(FABS, $4, ENULL); }
	| '@' K_FLOOR '(' e ')'		{ $$ = new(FLOOR, $4, ENULL); }
	| '@' K_HYPOT '(' e ',' e ')'	{ $$ = new(HYPOT, $4, $6); }
	| '@' K_LN '(' e ')'		{ $$ = new(LOG, $4, ENULL); }
	| '@' K_LOG '(' e ')'		{ $$ = new(LOG10, $4, ENULL); }
	| '@' K_POW '(' e ',' e ')'	{ $$ = new(POW, $4, $6); }
	| '@' K_SIN '(' e ')'		{ $$ = new(SIN, $4, ENULL); }
	| '@' K_SQRT '(' e ')'		{ $$ = new(SQRT, $4, ENULL); }
	| '@' K_TAN '(' e ')'		{ $$ = new(TAN, $4, ENULL); }
	| '@' K_DTR '(' e ')'		{ $$ = new(DTR, $4, ENULL); }
	| '@' K_RTD '(' e ')'		{ $$ = new(RTD, $4, ENULL); }
	| '@' K_RND '(' e ')'		{ $$ = new(RND, $4, ENULL); }
	| '@' K_ROUND '(' e ',' e ')'	{ $$ = new(ROUND, $4, $6); }
	| '@' K_IF  '(' e ',' e ',' e ')' { $$ = new(IF,  $4,new(',',$6,$8)); }

	| '@' K_PV  '(' e ',' e ',' e ')' { $$ = new(PV,  $4,new(':',$6,$8)); }
 	| '@' K_FV  '(' e ',' e ',' e ')' { $$ = new(FV,  $4,new(':',$6,$8)); }
 	| '@' K_PMT '(' e ',' e ',' e ')' { $$ = new(PMT, $4,new(':',$6,$8)); }
 
	| '@' K_HOUR '(' e ')'		{ $$ = new(HOUR, $4, ENULL); }
	| '@' K_MINUTE '(' e ')'	{ $$ = new(MINUTE, $4, ENULL); }
	| '@' K_SECOND '(' e ')'	{ $$ = new(SECOND, $4, ENULL); }
	| '@' K_MONTH '(' e ')'		{ $$ = new(MONTH, $4, ENULL); }
	| '@' K_DAY '(' e ')'		{ $$ = new(DAY, $4, ENULL); }
	| '@' K_YEAR '(' e ')'		{ $$ = new(YEAR, $4, ENULL); }
	| '@' K_NOW			{ $$ = new(NOW, ENULL, ENULL);}
	| '@' K_DTS '(' e ',' e ',' e ')'
					{ $$ = new(DTS, $4, new(',', $6, $8));}
	| NUMBER '.' NUMBER '.' NUMBER	{ $$ = new(DTS,
				new_const(O_CONST, (double) $1),
				new(',', new_const(O_CONST, (double) $3),
				new_const(O_CONST, (double) $5)));}
	| '@' K_TTS '(' e ',' e ',' e ')'
					{ $$ = new(TTS, $4, new(',', $6, $8));}
	| '@' K_STON '(' e ')'		{ $$ = new(STON, $4, ENULL); }
	| '@' K_EQS '(' e ',' e ')'	{ $$ = new(EQS, $4, $6); }
	| '@' K_DATE '(' e ')'		{ $$ = new(DATE, $4, ENULL); }
	| '@' K_DATE '(' e ',' e ')'	{ $$ = new(DATE, $4, $6); }
	| '@' K_FMT  '(' e ',' e ')'	{ $$ = new(FMT, $4, $6); }
	| '@' K_UPPER '(' e ')'		{ $$ = new(UPPER, $4, ENULL); }
	| '@' K_LOWER '(' e ')'		{ $$ = new(LOWER, $4, ENULL); }
	| '@' K_CAPITAL '(' e ')'	{ $$ = new(CAPITAL, $4, ENULL); }
	| '@' K_INDEX  '(' range ',' e ')'
		 { $$ = new(INDEX, new_range(REDUCE | INDEX, $4), $6); }
	| '@' K_INDEX  '(' e ',' range ')'
		 { $$ = new(INDEX, new_range(REDUCE | INDEX, $6), $4); }
	| '@' K_INDEX  '(' range ',' e ',' e ')'
		 { $$ = new(INDEX, new_range(REDUCE | INDEX, $4),
		    new(',', $6, $8)); }
	| '@' K_LOOKUP  '(' range ',' e ')'
		 { $$ = new(LOOKUP, new_range(REDUCE | LOOKUP, $4), $6); }
	| '@' K_LOOKUP  '(' e ',' range ')'
		 { $$ = new(LOOKUP, new_range(REDUCE | LOOKUP, $6), $4); }
	| '@' K_HLOOKUP  '(' range ',' e ',' e ')'
		 { $$ = new(HLOOKUP, new_range(REDUCE | HLOOKUP, $4),
		    new(',', $6, $8)); }
	| '@' K_HLOOKUP  '(' e ',' range ',' e ')'
		 { $$ = new(HLOOKUP, new_range(REDUCE | HLOOKUP, $6),
		    new(',', $4, $8)); }
	| '@' K_VLOOKUP  '(' range ',' e ',' e ')'
		 { $$ = new(VLOOKUP, new_range(REDUCE | VLOOKUP, $4),
		    new(',', $6, $8)); }
	| '@' K_VLOOKUP  '(' e ',' range ',' e ')'
		 { $$ = new(VLOOKUP, new_range(REDUCE | VLOOKUP, $6),
		    new(',', $4, $8)); }
	| '@' K_STINDEX  '(' range ',' e ')'
		 { $$ = new(STINDEX, new_range(REDUCE | STINDEX, $4), $6); }
	| '@' K_STINDEX  '(' e ',' range ')'
		 { $$ = new(STINDEX, new_range(REDUCE | STINDEX, $6), $4); }
	| '@' K_STINDEX  '(' range ',' e ',' e ')'
		 { $$ = new(STINDEX, new_range(REDUCE | STINDEX, $4),
		    new(',', $6, $8)); }
	| '@' K_EXT  '(' e ',' e ')'	{ $$ = new(EXT, $4, $6); }
	| '@' K_NVAL '(' e ',' e ')'	{ $$ = new(NVAL, $4, $6); }
	| '@' K_SVAL '(' e ',' e ')'	{ $$ = new(SVAL, $4, $6); }
	| '@' K_SUBSTR '(' e ',' e ',' e ')'
				{ $$ = new(SUBSTR, $4, new(',', $6, $8)); }
	|	'(' e ')'	{ $$ = $2; }
	|	'+' term	{ $$ = $2; }
	|	'-' term	{ $$ = new('m', $2, ENULL); }
	|	NUMBER		{ $$ = new_const(O_CONST, (double) $1); }
	|	FNUMBER		{ $$ = new_const(O_CONST, $1); }
	| '@'   K_PI		{ $$ = new(PI_, ENULL, ENULL); }
	|	STRING	        { $$ = new_str($1); }
	|	'~' term	{ $$ = new('!', $2, ENULL); }
	|	'!' term	{ $$ = new('!', $2, ENULL); }
	| '@' K_FILENAME '(' e ')'	{ $$ = new(FILENAME, $4, ENULL); }
	| '@' K_MYROW			{ $$ = new(MYROW, ENULL, ENULL);}
	| '@' K_MYCOL			{ $$ = new(MYCOL, ENULL, ENULL);}
	| '@' K_LASTROW			{ $$ = new(LASTROW, ENULL, ENULL);}
	| '@' K_LASTCOL			{ $$ = new(LASTCOL, ENULL, ENULL);}
	| '@' K_COLTOA '(' e ')'	{ $$ = new(COLTOA, $4, ENULL);}
	| '@' K_NUMITER			{ $$ = new(NUMITER, ENULL, ENULL);}
	| '@' K_ERR		{ $$ = new(ERR_, ENULL, ENULL); }
	|     K_ERR		{ $$ = new(ERR_, ENULL, ENULL); }
	| '@' K_BLACK		{ $$ = new(BLACK, ENULL, ENULL); }
	| '@' K_RED		{ $$ = new(RED, ENULL, ENULL); }
	| '@' K_GREEN		{ $$ = new(GREEN, ENULL, ENULL); }
	| '@' K_YELLOW		{ $$ = new(YELLOW, ENULL, ENULL); }
	| '@' K_BLUE		{ $$ = new(BLUE, ENULL, ENULL); }
	| '@' K_MAGENTA		{ $$ = new(MAGENTA, ENULL, ENULL); }
	| '@' K_CYAN		{ $$ = new(CYAN, ENULL, ENULL); }
	| '@' K_WHITE		{ $$ = new(WHITE, ENULL, ENULL); }
	;

/* expressions */
e:		e '+' e		{ $$ = new('+', $1, $3); }
	|	e '-' e		{ $$ = new('-', $1, $3); }
	|	e '*' e		{ $$ = new('*', $1, $3); }
	|	e '/' e		{ $$ = new('/', $1, $3); }
	|	e '%' e		{ $$ = new('%', $1, $3); }
	|	e '^' e		{ $$ = new('^', $1, $3); }
	|	term
	|	e '?' e ':' e	{ $$ = new('?', $1, new(':', $3, $5)); }
	|	e ';' e		{ $$ = new(';', $1, $3); }
	|	e '<' e		{ $$ = new('<', $1, $3); }
	|	e '=' e		{ $$ = new('=', $1, $3); }
	|	e '>' e		{ $$ = new('>', $1, $3); }
	|	e '&' e		{ $$ = new('&', $1, $3); }
	|	e '|' e		{ $$ = new('|', $1, $3); }
	|	e '<' '=' e	{ $$ = new('!', new('>', $1, $4), ENULL); }
	|	e '!' '=' e	{ $$ = new('!', new('=', $1, $4), ENULL); }
	|	e '<' '>' e	{ $$ = new('!', new('=', $1, $4), ENULL); }
	|	e '>' '=' e	{ $$ = new('!', new('<', $1, $4), ENULL); }
	|	e '#' e		{ $$ = new('#', $1, $3); }
	;

expr_list:	e		{ $$ = new(ELIST, ENULL, $1); }
	|	expr_list ',' e	{ $$ = new(ELIST, $1, $3); }
	;

range:		var ':' var	{ $$.left = $1; $$.right = $3; }
	| 	RANGE		{ $$ = $1; }
	;

var:		COL NUMBER	{ $$.vp = lookat($2, $1); $$.vf = 0; }
	|	'$' COL NUMBER	{ $$.vp = lookat($3, $2);
					$$.vf = FIX_COL; }
	|	COL '$' NUMBER	{ $$.vp = lookat($3, $1);
					$$.vf = FIX_ROW; }
	|	'$' COL '$' NUMBER { $$.vp = lookat($4, $2);
					$$.vf = FIX_ROW | FIX_COL; }
	|	VAR		{ $$ = $1.left; }
	;

var_or_range:	range		{ $$ = $1; }
	|	var		{ $$.left = $1; $$.right = $1; }
	;

num:		NUMBER		{ $$ = (double) $1; }
	|	FNUMBER		{ $$ = $1; }
	|	'-' num		{ $$ = -$2; }
	|	'+' num		{ $$ = $2; }
	;

strarg:		STRING		{ $$ = $1; }
	|	var		{
				    char *s, *s1;
				    size_t l;
				    s1 = $1.vp->label;
				    if (!s1)
					s1 = "NULL_STRING";
				    l = strlen(s1) + 1;
				    s = scxmalloc(l);
				    strlcpy(s, s1, l);
				    $$ = s;
				}
  	;

/* allows >=1 'setitem's to be listed in the same 'set' command */
setlist :
	|	setlist	setitem
	;

/* things that you can 'set' */
setitem	:	K_AUTO			{ setauto(1); }
	|	K_AUTOCALC		{ setauto(1); }
	|	'~' K_AUTO		{ setauto(0); }
	|	'~' K_AUTOCALC		{ setauto(0); }
	|	'!' K_AUTO		{ setauto(0); }
	|	'!' K_AUTOCALC		{ setauto(0); }
	|	K_BYCOLS		{ setorder(BYCOLS); }
	|	K_BYROWS		{ setorder(BYROWS); }
	|	K_OPTIMIZE		{ optimize = 1; }
	|	'~' K_OPTIMIZE		{ optimize = 0; }
	|	'!' K_OPTIMIZE		{ optimize = 0; }
	|	K_NUMERIC		{ numeric = 1; }
	|	'~' K_NUMERIC		{ numeric = 0; }
	|	'!' K_NUMERIC		{ numeric = 0; }
	|	K_PRESCALE		{ prescale = 0.01; }
	|	'~' K_PRESCALE		{ prescale = 1.0; }
	|	'!' K_PRESCALE		{ prescale = 1.0; }
	|	K_EXTFUN		{ extfunc = 1; }
	|	'~' K_EXTFUN		{ extfunc = 0; }
	|	'!' K_EXTFUN		{ extfunc = 0; }
	|	K_CELLCUR		{ showcell = 1; }
	|	'~' K_CELLCUR		{ showcell = 0; }
	|	'!' K_CELLCUR		{ showcell = 0; }
	|	K_TOPROW		{ showtop = 1; }
	|	'~' K_TOPROW		{ showtop = 0; }
	|	'!' K_TOPROW		{ showtop = 0; }
	|	K_AUTOINSERT		{ autoinsert = 1; }
	|	'~' K_AUTOINSERT	{ autoinsert = 0; }
	|	'!' K_AUTOINSERT	{ autoinsert = 0; }
	|	K_AUTOWRAP		{ autowrap = 1; }
	|	'~' K_AUTOWRAP		{ autowrap = 0; }
	|	'!' K_AUTOWRAP		{ autowrap = 0; }
	|	K_CSLOP			{ cslop = 1; FullUpdate++; }
	|	'~' K_CSLOP		{ cslop = 0; FullUpdate++; }
	|	'!' K_CSLOP		{ cslop = 0; FullUpdate++; }
	|	K_COLOR			{ color = 1;
					  if (usecurses && has_colors()) {
					    color_set(1, NULL);
					    bkgd(COLOR_PAIR(1) | ' ');
					    FullUpdate++;
					  }
					}
	|	'!' K_COLOR		{ color = 0;
					  if (usecurses && has_colors()) {
					    color_set(0, NULL);
					    bkgd(COLOR_PAIR(0) | ' ');
					  }
					}
	|	'~' K_COLOR		{ color = 0;
					  if (usecurses && has_colors()) {
					    color_set(0, NULL);
					    bkgd(COLOR_PAIR(0) | ' ');
					  }
					}
	|	K_COLORNEG		{ colorneg = 1; }
	|	'!' K_COLORNEG		{ colorneg = 0; }
	|	'~' K_COLORNEG		{ colorneg = 0; }
	|	K_COLORERR		{ colorerr = 1; }
	|	'!' K_COLORERR		{ colorerr = 0; }
	|	'~' K_COLORERR		{ colorerr = 0; }
	|	K_BRAILLE		{ braille = 1; }
	|	'!' K_BRAILLE		{ braille = 0; }
	|	'~' K_BRAILLE		{ braille = 0; }
	|	K_BACKUP		{ dobackups = 1; }
	|	'!' K_BACKUP		{ dobackups = 0; }
	|	'~' K_BACKUP		{ dobackups = 0; }
	|	K_MOUSE			{ mouseon(); }
	|	'!' K_MOUSE		{ mouseoff(); }
	|	'~' K_MOUSE		{ mouseoff(); }
	|	K_ITERATIONS '=' NUMBER	{ setiterations($3); }
	|	K_TBLSTYLE '=' NUMBER	{ tbl_style = $3; }
	|	K_TBLSTYLE '=' K_TBL	{ tbl_style = TBL; }
	|	K_TBLSTYLE '=' K_LATEX	{ tbl_style = LATEX; }
	|	K_TBLSTYLE '=' K_SLATEX	{ tbl_style = SLATEX; }
	|	K_TBLSTYLE '=' K_TEX	{ tbl_style = TEX; }
	|	K_TBLSTYLE '=' K_FRAME	{ tbl_style = FRAME; }
	|	K_RNDTOEVEN		{ rndtoeven = 1; FullUpdate++; }
	|	'!' K_RNDTOEVEN		{ rndtoeven = 0; FullUpdate++; }
	|	'~' K_RNDTOEVEN		{ rndtoeven = 0; FullUpdate++; }
	|	K_CRACTION '=' NUMBER	{ craction = $3; }
	|	K_ROWLIMIT '=' NUMBER	{ rowlimit = $3; }
	|	K_COLLIMIT '=' NUMBER	{ collimit = $3; }
	|	K_PAGESIZE '=' NUMBER	{ pagesize = $3; }
	|	K_SCRC			{ scrc++; }
	|	K_LOCALE		{
#ifdef USELOCALE
					  struct  lconv *locstruct;
					  char    *loc;

					  loc = setlocale(LC_ALL, "");
					  if (loc != NULL) {
					    locstruct = localeconv();
					    dpoint = (locstruct->decimal_point)[0];
					    thsep = (locstruct->thousands_sep)[0];
					  }
					  else {
					    dpoint = '.';
					    thsep = ',';
					  }
					  FullUpdate++;
#else
					  error("Locale support not available");
#endif
					}
	|	'!' K_LOCALE		{
					  dpoint = '.';
					  thsep = ',';
					  FullUpdate++;
					}
	|	'~' K_LOCALE		{
					  dpoint = '.';
					  thsep = ',';
					  FullUpdate++;
					}
  	;

/* types of errors, to 'goto' */
errlist :	K_ERROR range		{ num_search((double)0,
					  $2.left.vp->row, $2.left.vp->col,
					  $2.right.vp->row, $2.right.vp->col,
					  CELLERROR, 0); }
	|	K_ERROR			{ num_search((double)0, 0, 0,
					  maxrow, maxcol, CELLERROR, 0); }
	|	K_INVALID range		{ num_search((double)0,
					  $2.left.vp->row, $2.left.vp->col,
					  $2.right.vp->row, $2.right.vp->col,
					  CELLINVALID, 0); }
	|	K_INVALID		{ num_search((double)0, 0, 0,
					  maxrow, maxcol, CELLINVALID, 0); }
	;
