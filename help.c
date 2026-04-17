/*
 * Help functions for sc 
 * R. Bond, 1988
 * J. Buhrt 1990
 * $Revision: 7.16 $
 */

#include <stdlib.h>
#ifdef QREF
#include <stdio.h>
static char	*header = " Quick Reference";
static char	*revision = "$Revision: 7.16 $";
#else
#include <limits.h>
#include "compat.h"
#include "sc.h"
#endif /* QREF */

static char *intro[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" Overview:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
" A:   This overview",
" B:   Toggle Options",
" C:   Set Options",
" D:   Cursor movement commands",
" E:   Cell entry and editing commands",
" F:   Line Editing",
" G:   File commands",
" H:   Row and column commands",
" I:   Range commands",
" J:   Miscellaneous commands",
" K:   Variable names/Expressions",
" L:   Range functions",
" M:   Numeric functions",
" N:   String functions",
" O:   Financial functions",
" P:   Time and date functions",
" ",
" Q:   Return to main spreadsheet",
(char *)0
};

static char *toggleoptions[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" B: Toggle Options",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     ^To  Toggle options. Toggle one option selected by o:",
"          a    Recalculate automatically or on ``@'' commands.",
"          o    Optimize expressions upon entry if enabled.",
"          c    Current cell highlighting enable/disable.",
"          e    External function execution enable/disable.",
"          l    Autolabeling defined cells enable/disable.",
"          n    If enabled, a digit starts a numeric value.",
"          t    Top line display enable/disable.",
"          b    Braille-friendly cursor positioning enable/disable.",
"          s    Color slop enable/disable.",
"          C    Color display enable/disable.",
"          N    Cycle color on cells with negative numbers enable/disable.",
"          E    Color cells with errors enable/disable.",
"          i    Autoinsert (insert mode after newline) enable/disable.",
"          w    Autowrap (wrap during newline action) enable/disable.",
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
"          x    Encrypt/decrypt database and listing files.",
#else
"          x    Encrypt/decrypt database and listing files (Not available).",
#endif
"          $    Dollar prescale.  If enabled, all numeric constants",
"               (not expressions) entered are multipled by 0.01.",
"          r    Newline action.  Toggle between no action, move down",
"               after entry and move right after entry.",
"          z    Set the newline action limits to the current row and column",
"            (for r && z see also set rowlimit=n, collimit=n)",
(char *)0
};

static char *setoptions[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" C: Set Options",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     S  Set options.  Options include:",
"          autocalc      Automatic recalculation.",
"          byrows        Recalculate in row order. (default)",
"          bycols        Recalculate in column order.",
"          optimize      Optimize expressions upon entry. (default off)",
"          numeric       A leading digit starts a numeric value.",
"          prescale      Multiply entered numeric constants by 0.01.",
"          extfun        Enable external function execution via @ext.",
"          autoinsert    Enter insert mode after a newline action.",
"          autowrap      Wrap around the spreadsheet during newline action.",
"          cslop         Color slop (show color of trailing space).",
"          color         Enable color display.",
"          colorneg      Cycle color on cells with negative numbers.",
"          colorerr      Color cells with errors using color 3.",
"          braille       Braille-friendly cursor positioning.",
"          backup        Make backup files on save.",
"          locale        Use locale-dependent numeric formatting.",
"          scrc          Load ./.scrc even when not the current directory.",
"          iterations=n  Set the number of iterations allowed. (10)",
"          tblstyle=xx   Set ``T'' output style to: 0 (none), tex,",
"                        latex, slatex, tbl, or frame.",
"          rndtoeven     Round *.5 to nearest even number instead of",
"                        always up.",
"          rowlimit=n    Set the remembered row limit for newline action.",
"          collimit=n    Set the remembered column limit for newline action.",
"                        (rowlimit and collimit can both be set by ^Tz)",
"          craction=n    Direction after newline action (0=none, 1=down,",
"                        2=right).",
"          pagesize=n    Set the page size used by ^F, ^B, J, K.",
(char *)0
};

static char *cursor[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" D: Cell cursor movement (always OK):",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     ^N ^P      Down, up",
"     j k l h    Down, up, right, left",
"     Arrow keys (if the terminal and termcap support them.)",
"     ^Ed        Go to end of range.  Follow ^E by a direction indicator",
"                such as ^P or j.",
" ",
"     J K L H    Down, up, right, left by 1/2 pages (or 'pagesize' rows)",
"     SPACE      Forward",
"     ^H         Back",
"     TAB        Forward, otherwise starts/ends a range",
"     ^          Up to row 0 of the current column.",
"     #          Down to the last valid row of the current column.",
"     0          Back to column A.  Preface with ^U if numeric mode.",
"     $          Forward to the last valid column of the current row.",
"     b          Back then up to the previous valid cell.",
"     w          Forward then down to the next valid cell.",
"     g          Go to a cell.  Cell name, range name, quoted string,",
"                a number, 'error', or 'invalid' to specify which cell.",
"     ` '        Go to a marked cell (see help screen e for more info.",
(char *)0
};


static char *cell[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" E: Cell entry and editing commands:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     =    Enter a numeric constant or expression.",
"     <    Enter a left justified string or string expression.",
#if defined(QREF) && defined(TROFF)
"     \\\\    Enter a centered label.",
#else
"     \\    Enter a centered label.",
#endif
"     >    Enter a right justified string or string expression.",
"     e    Edit the current cell's numeric value.",
"     E    Edit the current cell's string part.",
"     F    Assign a format to the current cell's numeric value.",
"     x    Clear the current cell.",
"     m    Followed by any lowercase letter, marks the current cell",
"          with that letter.",
"     cm   Copy a cell marked with letter m to the current cell.",
"          Use ``c.'' instead to pick the destination interactively.",
"     +    Increment numeric part",
"     -    Decrement numeric part",
"  RETURN  Enter insert mode if the input line was empty (ESC to edit)",
" ",
"     In numeric mode, a decimal digit, ``+'', ``-'', and ``.'' all start",
"     a new numeric constant or expression.",
(char *)0
};


static char *vi[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" F: Line Editor",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     Hitting the ESC key while entering any command on the top line",
"     will start a one-line vi-style editor.  Supported commands:",
" ",
"     ESC q        Abort command entry.",
"     h l          Move cursor forward, backward.",
"     0 $          Move cursor to the beginning, end of the line.",
"     b w          Move cursor forward/back one word.",
"     e            Move cursor to next end-of-word.",
"     fc           Move cursor to character c.",
"     tc           Move the cursor to the character before c.",
"     i a          Enter insert mode before/after the cursor.",
"     I A          Move to column 0/end of line and enter insert mode.",
"     x X          Delete the character under/before the cursor.",
"     rc           Replace the character under the cursor with c.",
"     cm dm        Change/Delete - m = b,e,f,h,l,t or w.",
"     R            Enter replace (overstrike) mode.",
"     s            Delete character under cursor and enter insert mode.",
"     + j         Forward through the command history.",
"     - k         Backward through the command history.",
"     / ?         Search backward/forward through history for a string.",
"     n N          Repeat last history search (N = opposite direction).",
"     . u          Repeat/undo the last command.",
"     ^V           Enter navigate mode.  Another ^V enters current cell address.",
"     ^W           Type, in the command line, the current cell's expression.",
"     ^A           Type, in the command line, the current cell's numeric value.",
(char *)0
};

static char *file[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" G: File commands:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     G    Get a new database from a file. ",
"     M    Merge a new file into the current database.",
"     R    Merge a macro file (from the macro directory if set).",
"     D    Set the macro directory.",
"     A    Set the autorun macro file.",
"     P    Put the current database into a file.",
"     W    Write a listing of the current database into a file in",
"          a form that matches its appearance on the screen.",
"     T    Write a listing of the current database to a file, but",
"          put delimiters between each pair of fields.",
"          Optionally brackets output with control lines for ``tbl'',",
"          ``LaTeX'', ``SLaTex'', or ``TeX''.",
" ",
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
"     If encryption mode is set, file I/O will be encrypted/decrypted.",
"     ``\"| program\"'' for a file name will pipe (unencrypted) output to",
#else
"     ``\"| program\"'' for a file name will pipe output to",
#endif
"     a program for Put, Write and Table.  If a cell name is used",
"     as the file name, the cell's string part will be used as the",
"     file name.",
(char *)0
};


static char *row[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" H: Row and column commands:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     ir, ic          Insert a new, empty row (column)",
"     or, oc          Open a row (column), like ir/ic then move to it",
"     ar, ac          Append a new copy of the current row (column)",
"     dr, dc          Delete the current row (column)",
"     yr, yc          Yank the current row (column) into the delete buffer",
"     pr, pc          Pull deleted cells back, inserting rows or columns.",
"     pp, pm, pf,     Pull deleted cells without inserting:",
"     px, pt, pC, p.  p=paste (overwrite), m=merge (no erase first),",
"                     f=format-only, x=exchange, t=transpose, C=copy",
"                     (adjust refs), .=navigate mode to pick destination.",
"     vr, vc          Remove expressions from the affected rows (cols),",
"                     leaving only the values.",
"     Zr, Zc          Hide (``zap'') the current row (column).",
"     ZZ              Save and exit (write current file if modified).",
"     sr, sc          Show hidden rows (columns).",
"     f               Set the output format to be used with the values of",
"                     each cell in this column.  Enter field width and",
"                     number of fractional digits.  A preceding count can",
"                     be used to change more than one column.",
" ",
"     Commands which move or copy cells also modify the row and column ",
"     references in the new cell expressions.  Use ``fixed'' or the",
"     ``$'' style cell reference to supress the change.",
" ",
"     @myrow, @mycol        return the row or column of the current cell",
"     @lastrow, @lastcol    return the last row or column of the spreadsheet",
(char *)0
};


static char *range[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" I: Range commands:",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     rx   Clear a range. ",
"     rv   Remove the expressions from a range of cells, leaving ",
"          just the values.",
"     rc   Copy a source range to a destination range.",
"     rm   Move a source range to a destination cell; references to the",
"          moved cells are updated to track the new addresses.",
"     ry   Yank a range into the delete buffer; the cells are not erased.",
"     rf   Fill a range with constant values starting with a given",
"          value and increasing by a given increment.",
"     rd   Assign a name to a cell or a range of cells.  Give the",
"          the name, surrounded by quotes, and either a cell name such",
"          as ``A10'' or a range such as ``a1:b20''.",
"     rl   Locks a cell or a range of cells, i.e makes it unchangeable.",
"     rU   Unlocks a locked cell, i.e makes it changeable.",
"     rs   Sort a range.  Give the range and an optional sort criteria",
"          string, e.g. \"+#a\" (ascending, numeric, column a).",
"     rS   Show the currently defined named ranges, framed ranges, and",
"          color definitions/ranges.  Output is piped to PAGER (less).",
"     ru   Use this command to undefine a previously defined range name.",
"     rF   Assign a format string to a range of cells.",
"     rr   Frame commands.  rrt/rrb/rrl/rrr set the top/bottom/left/right",
"          frame; rra frames a range; rru unframes it.",
"     rC   Assign a color number to a range of cells.",
"     r{ r} r|  Left/right justify or center strings in a range.",
" ",
"     Range operations affect a rectangular region on the screen",
"     defined by the upper left and lower right cells in the region.",
"     A range is specified by giving the cell names separated by ``:'',",
"     such as ``a20:k52''.  Another way to refer to a range is to use",
"     a name previously defined using ``rd''.",
(char *)0
};


static char *misc[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" J: Miscellaneous commands:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     Q q ^C   Exit from the program.",
"     ^G ESC   Abort entry of the current command.",
"     ?        Help",
"     !        Shell escape.  Enter a command to run.  ``!!'' repeats",
"              the last command.  Just ``!'' starts an interactive shell.",
"     ^L       Redraw the screen.",
"     ^R       Redraw the screen.  Highlight cells with values but no",
"              expressions.",
"     ^X       Redraw the screen.  Show formulas, not values.",
"     z<CR>    Move the current row to the top of the screen.",
"     z.       Move the current row to the center of the screen.",
"     z|       Move the current column to the center of the screen.",
"     zc       Center the current cell horizontally and vertically.",
"     C        Define color pair 1-8.  See also ``rC'' and ``^TC''.",
"     ~        Abbreviation management.  Enter abbreviation and expansion",
"              in quotes, separated by a single space.",
"     \"        Select a named delete buffer (a-z or 0-9) for the next",
"              delete, yank, or pull.",
"     *        Notes: followed by a (add), d (delete), s (show), or *",
"              (go to the cell linked as a note).",
"     @        Recalculate the spreadsheet.",
"     TAB      When the character cursor is on the top line TAB can be used",
"              to start or stop the display of the default range.",
(char *)0
};

static char *var[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" K: Variable names:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     K20    Row and column can vary on copies.",
"     $K$20  Row and column stay fixed on copies.",
"     $K20   Row can vary; column stays fixed on copies.",
"     K$20   Row stays fixed; column can vary on copies.",
"     fixed  holds following expession fixed on copies.",
"     Cells and ranges can be given a symbolic name via ``rd''.",
" ",
" Expressions:",
"     -e      Negation                e<=e  Less than or equal",
"     e+e     Addition                e=e   Equal",
"     e-e     Subtraction             e!=e  Not Equal",
"     e*e     Multiplication          e>=e  Greater than or equal",
"     e/e     Division                e>e  Greater than",
"     e%e     Modulo                  e<e  Less than",
"     e^e     Exponentiation          e&e  Boolean operator AND.",
"     ~e      Boolean operator NOT    e|e  Boolean operator OR",
"     e?e1:e2  or @if(e,e1,e2)",
"             Conditional: If e is non zero then then e1, else e2.",
"     Terms may be constants, variables, and parenthesized expressions.",
(char *)0
};

static char *rangef[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" L: Range functions:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     @sum(r)           Sum all valid cells in the range.",
"     @prod(r)          Multiply together all valid cells in the range.",
"     @avg(r)           Average all valid cells in the range.",
"     @count(r)         Count all valid cells in the range.",
"     @max(r)           Return the maximum value in the range.",
"     @min(r)           Return the minimum value in the range.",
"     @stddev(r)        Return the sample standard deviation of ",
"                       the cells in the range.",
"     @index(e,r) @stindex(e,r)",
"                       Return the numeric (string) value of the cell at",
"                       index e into range r.",
"     @lookup(e,r) @hlookup(e,r,n) @vlookup(e,r,n)",
"                       Search through the range r for a value that",
"                       matches e.  If e is numeric, the last value <= e",
"                       matches; if string, an exact match is required.",
"                       @lookup searches a single row (column) and returns",
"                       the value from the next column (row); @hlookup",
"                       (@vlookup) searches the first row (column) in r and",
"                       returns the value n columns (rows) from the match.",
(char *)0
};

static char *numericf[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" M: Numeric functions:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     @atan2(e1,e2)     Arc tangent of e1/e2.",
"     @ceil(e)          Smallest integer not less than e.",
"     @eqs(se1,se2)     1 if string expr se1 has the same value as se2.",
"     @exp(e)           Exponential function of e.",
"     @abs(e) @fabs(e)  Absolute value of e.",
"     @floor(e)         The largest integer not greater than e.",
"     @hypot(x,y)       Sqrt(x*x+y*y).",
"     @max(e1,e2,...)   The maximum of the values of the e's.",
"     @min(e1,e2,...)   The minimum of the values of the e's",
"     @nval(se,e)       The numeric value of a named cell.",
"     pi       @pi      A constant quite close to pi.",
"     @pow(e1,e2)       e1 raised to the power of e2.",
"     @rnd(e)           Round e to the nearest integer.",
"     @round(e,n)       Round e to n decimal places.",
"     @sqrt(e)          Square root of e.",
"     @ston(se)         Convert string expr se to a numeric",
"     @ln(e)   @log(e)           Natural/base 10 logarithm of e.",
"     @dtr(e)  @rtd(e)           Convert degrees to/from radians.",
"     @cos(e)  @sin(e)  @tan(e)  Trig functions of radian arguments.",
"     @asin(e) @acos(e) @atan(e) Inverse trig function.",
(char *)0
};

static char *stringf[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" N: String functions:",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     #                 Concatenate strings.  For example, the",
"                       string expression ``A0 # \"zy dog\"'' yields",
"                       ``the lazy dog'' if A0 is ``the la''.",
"     @substr(se,e1,e2) Extract characters e1 through e2 from the",
"                       string expression se.  For example,",
"                       ``@substr(\"Nice jacket\" 4, 7)'' yields ",
"                       ``e ja''.",
"     @fmt(se,e)        Convert a number to a string using sprintf(3).",
"                       For example,  ``@fmt(\"*%6.3f*\",10.5)'' yields",
"                       ``*10.500*''.  Use formats are e, E, f, g, and G.", 
"     @sval(se,e)       Return the string value of a cell selected by name.",
"     @ext(se,e)        Call an external function (program or",
"                       script).  Convert e to a string and append it",
"                       to the command line as an argument.  @ext yields",
"                       a string: the first line printed to standard",
"                       output by the command.",
"     @coltoa(e)        Return the column letter(s) from the passed number",
"     @upper(e) @lower(e)   Return the string in upper/lower case",
"     @capital(e)       Return the string with words in upper case",
"     String expressions are made up of constant strings (characters",
"     surrounded by quotes), variables, and string functions.",
(char *)0
};


static char *finf[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" O: Financial functions:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     @pmt(e1,e2,e3)    @pmt(60000,.01,360) computes the monthly",
"                       payments for a $60000 mortgage at 12%",
"                       annual interest (.01 per month) for 30",
"                       years (360 months).",
" ",
"     @fv(e1,e2,e3)     @fv(100,.005,36) computes the future value",
"                       of 36 monthly payments of $100 at 6%",
"                       interest (.005 per month).  It answers the",
"                       question:  ``How much will I have in 36",
"                       months if I deposit $100 per month in a",
"                       savings account paying 6% interest com-",
"                       pounded monthly?''",
" ",
"     @pv(e1,e2,e3)     @pv(1000,.015,36) computes the present",
"                       value of an ordinary annuity of 36",
"                       monthly payments of $1000 at 18% annual",
"                       interest.  It answers the question: ``How",
"                       much can I borrow at 18% for 30 years if I",
"                       pay $1000 per month?''",
(char *)0
};


static char *timef[] = {
" ",
#if defined(QREF) && defined(TROFF)
".SH",
#endif
" P: Time and date functions:",
" ",
#if defined(QREF) && defined(TROFF)
".Lp",
#endif
"     @now              Return the time encoded in seconds since 1970.",
"     @dts(e1,e2,e3)    Return e1/e2/e3 encoded in seconds since 1970.",
"                       Either m/d/y or y/m/d may be used (year must",
"                       include the century), although y/m/d is preferred.",
"     @tts(h,m,s)       Return h:m:s encoded in seconds since midnight.",
" ",
"     All of the following take an argument expressed in seconds:",
" ",
"     @date(e)          Convert the time in seconds to a date",
"                       string 24 characters long in the following",
"                       form: ``Sun Sep 16 01:03:52 1973''.  Note",
"                       that you can extract pieces of this fixed format",
"                       string with @substr.",
"     @year(e)          Return the year.  Valid years begin with 1970.",
"     @month(e)         Return the month: 1 (Jan) to 12 (Dec).",
"     @day(e)           Return the day of the month: 1 to 31.",
"     @hour(e)          Return the number of hours since midnight: 0 to 23.",
"     @minute(e)        Return the number of minutes since the",
"                       last full hour: 0 to 59.",
"     @second(e)        Return the number of seconds since the",
"                       last full minute: 0 to 59.",
(char *)0
};

#ifndef QREF
static	int	pscreen(char *screen[]);

void
help(void) {
    int option;
    char **ns = intro;

    while((option = pscreen(ns)) != 'q' && option != 'Q') {
    	switch (option) {
	case 'a': case 'A': ns = intro; break;
	case 'b': case 'B': ns = toggleoptions; break;
	case 'c': case 'C': ns = setoptions; break;
	case 'd': case 'D': ns = cursor; break;
	case 'e': case 'E': ns = cell; break;
	case 'f': case 'F': ns = vi; break;
	case 'g': case 'G': ns = file; break;
	case 'h': case 'H': ns = row; break;
	case 'i': case 'I': ns = range; break;
	case 'j': case 'J': ns = misc; break;
	case 'k': case 'K': ns = var; break;
	case 'l': case 'L': ns = rangef; break;
	case 'm': case 'M': ns = numericf; break;
	case 'n': case 'N': ns = stringf; break;
	case 'o': case 'O': ns = finf; break;
	case 'p': case 'P': ns = timef; break;
	default: ns = intro; break;
	}
    }
    FullUpdate++;
    (void) move(1,0);
    (void) clrtobot();
}

static int
pscreen(char *screen[])
{
    int lineno;
    int dbline;

    (void) move(1,0);
    (void) clrtobot();
    dbline = 1;
    for (lineno = 0; screen[lineno]; lineno++) {
	(void) move(dbline++, 4);
	(void) addstr (screen[lineno]);
	(void) clrtoeol();
    }
    (void) move(0,0);
    (void) printw("Which Screen? [a-p, q]");
    (void) clrtoeol();
    (void) refresh();
    return(nmgetch());
}
#else
static char	** pages[] = { intro, toggleoptions, setoptions, cursor, cell, vi,
			file, row, range, misc, var, rangef, numericf, stringf,
			finf, timef, NULL};

int
main() {
    int	lineno;
    char	***pagep = pages;
#ifdef TROFF
    int	pageno = 0;

    puts(".nr PS 12");
    puts(".nr VS 14");
    puts(".nr HM 1i");
    puts(".nr FM 1i");
    puts(".nr PO 0.5i");
    printf(".EH '%s%s''%s'\n", SCNAME, header, revision);
    printf(".OH '%s%s''%s'\n", SCNAME, header, revision);
    puts(".EF ''%''");
    puts(".OF ''%''");
    puts(".de Lp");
    puts(".LP");
    puts(".ft CW");
    puts(".na");
    puts(".nf");
    puts("..");
    puts(".P1");
    puts(".LP");
#endif

    while (*pagep)
    {
#ifndef TROFF
	(void) fputs(SCNAME, stdout);
	(void) fputs(header, stdout);
	(void) printf("\n");
	(void) puts(revision);
#endif

	for (lineno = 0; (*pagep)[lineno]; lineno++) {
		(void) puts((*pagep)[lineno]);
	}
#if !defined(TROFF)
	(void) putchar('\f');
#endif
	pagep++;
#ifdef TROFF
	pageno++;
	if (!(pageno%2)) puts(".bp");
#endif
    }
    return 0;
}
#endif /* QREF */
