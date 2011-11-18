# Tcl/Tk 8.5 Manual Fathead #

Programming reference for Tcl/Tk 8.5. Based on <http://www.tcl.tk/man/tcl8.5/>.

Presently ignores the `namespace`, `details`, `type`, and `lang` fields described in the
programming data file format section of the fathead readme. Provides only the first
sentence of the `description` given for each command in the manual; this tends to be
just right to confirm or suggest what the command is for, although the whole first
paragraph would provide more `details`. The `synopsis` may contain multiple lines,
separated by `<br />`.

Some pages in the manual do not document specific commands. Most such pages are omitted.

Parsing is accomplished with gnarly and not at all fragile regular expressions. Cowabunga.
