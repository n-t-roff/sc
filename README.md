### sc Spreadsheet Calculator

`sc` is a free curses-based spreadsheet program that uses key bindings similar to vi and less.
For more information on `sc` please see the
[`README`](https://github.com/n-t-roff/sc/blob/master/README)
file.
This is a fork of the latest source code version 7.16 from September 2002
which can be found at
[http://www.ibiblio.org/pub/Linux/apps/financial/spreadsheet/sc-7.16.tar.gz](http://www.ibiblio.org/pub/Linux/apps/financial/spreadsheet/sc-7.16.tar.gz).
The following
[changes](https://github.com/n-t-roff/sc/blob/master/CHANGES-git)
had been made:

* Many bugs had been found, all of them are fixed now.
  (For reporting bugs please use the
  [issue list](https://github.com/n-t-roff/sc/issues).)
* All `gcc` and most `clang` `-Wall` and `-Werror` compiler warnings fixed.
* Made `UBSAN` and `ASAN` clean.
* All `sprintf`, `strcpy`, and `strcat` replaced
  with `snprintf`, `strlcpy`, and `strlcat`.
* `./configure` script added and tested on BSD,
  Linux, and Solaris.

Functional extensions:

* By default `sc` made a backup of the database file
  before overwriting it.
  This could only be changed at compile time.
  There are now options `backup` and `!backup` to control this.
* Configured at compile time, `sc` always did update the history
  file `~/.sc_history`.
  It is now possible to change this filename with the `.scrc`
  command `histfile` or save no history at all with an empty
  command argument.
* Command `set mouse` (e.g. in `.scrc`) enables

  * Selecting cells with the left mouse button
  * Vertical scrolling with the mouse wheel
  * Selecting the edit line position with the left mouse button

  (In this case the shift key needs to be pressed for
  &lsquo;cut and paste&rsquo;.
  All `sc` functions work well with NetBSD and Solaris curses
  but using the mouse requires ncurses.
  Using the mouse _wheel_ additionally requires
  ncurses version &ge; 6.)
* Add 'N' command to repeat last search in opposite direction

#### Documentation

Documentation is provided in short form integrated in `sc`
accessible with the `?` key.
A complete documentation is available as a
[manual page](https://github.com/n-t-roff/sc/blob/master/sc.doc).
For better navigation
[HTML](http://n-t-roff.github.io/sc.1.html)
and
[PDF](http://n-t-roff.github.io/sc.1.pdf)
versions with table of contents and hyperlinks had been
created (using the
[-man](http://n-t-roff.github.io/heirloom/doctools/man.7.html)
macros of
[Heirloom troff](http://n-t-roff.github.io/heirloom/doctools.html)).

#### Getting the source

The source can be cloned with
```sh
git clone https://github.com/n-t-roff/sc.git
```
and updated later with
```sh
git pull
```
#### Compiling and Installation

Defaults for compiling and installation are set in the
[`Makefile.in`](https://github.com/n-t-roff/sc/blob/master/Makefile.in).
A system dependent makefile is generated with
```sh
./configure
```
If you'd like to do an out-of-source build, and your version of make supports VPATH (eg GNU make)
then you can do the following:
```sh
cd /path/to/obj
/path/to/source/configure # (full or relative path)
```
The source is compiled with
```sh
make
```
(the `curses` or `ncurses` header files need to be installed,
else `make` failes).

If `make` failes it may be possible that `./configure` did not work
correctly with the installed `/bin/sh`.
If `ksh` is installed in such cases using
```sh
make distclean
ksh configure
make
```
can solve the problem.
(Alternatively `sh` in `configure`s first line can be changed to `ksh`.)

The result is installed with
```sh
make install
```
to the base directory `/usr/local`.
This path can be changed in the
[`Makefile.in`](https://github.com/n-t-roff/sc/blob/master/Makefile.in).
All generated files are removed with
```sh
make distclean
```
A new target, `clobber`, has been added, which removes all `make` generated targets (not the Makefile nor other `configure` output)
```sh
make clobber
```
The package can be uninstalled with
```sh
make uninstall
```
