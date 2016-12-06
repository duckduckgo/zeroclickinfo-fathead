#!/bin/bash

tclsh parse.tcl "http://www.tcl.tk/man/tcl8.5/UserCmd/" download/UserCmd/*.htm > output.txt
tclsh parse.tcl "http://www.tcl.tk/man/tcl8.5/TclCmd/" download/TclCmd/*.htm >> output.txt
tclsh parse.tcl "http://www.tcl.tk/man/tcl8.5/TkCmd/" download/TkCmd/*.htm >> output.txt

LC_ALL=C sort output.txt -o output.txt
