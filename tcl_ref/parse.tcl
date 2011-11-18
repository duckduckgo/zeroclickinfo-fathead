#!/usr/bin/tclsh

package require Tcl 8.5

if {[llength $argv] < 2} {
	puts stderr "Usage: parse.tcl URL_PREFIX FILE1 ?FILE2 FILE3 ...?"
	exit 1
}

set url_prefix [lindex $argv 0]

foreach arg [lrange $argv 1 end] {
	
	#
	# Load the file. Report and skip if we can't.
	#
	if {[catch {open $arg} f]} {
		puts stderr $f
		continue
	}
	set t [read $f]
	close $f
	
	#
	# Get the page name.
	#
	# For most commands, the name is followed by a dash (" - ") and a terse description.
	# In some cases, however, multiple commands share a page and appear listed with commas.
	# In such cases, we ignore all but the first (otherwise, hard to find appropriate desc.)
	#
	# Regexp: find the NAME header; take everything up to a "," or " - " from the next line.
	#
	set name {}
	regexp {<H3><A NAME="M\d+">NAME</A></H3>\n((?:(?! - )[^,])*?)} $t match name
	
	#
	# Get the synopsis.
	#
	# Some pages don't have synopses. Most of those do not document specific commands, and
	# should be omitted from fetch.sh. Some pages have multi-line synopses; some multi-line
	# synopses are appropriate, since they show different forms of the same command. Some
	# multi-line synopses are inappropriate, since they document different commands. At
	# present, we show all multi-line synopses in their entirety, separated with <br />.
	# Most pages with inappropriate multi-line synopses can probably be omitted.
	#
	# Regexp: find the SYNOPSIS header; starting at next line, take everything up to the next header.
	# Tags are stripped (most would be helpful to keep). Newlines are replaced with <br />.
	#
	set synopsis {}
	regexp {<H3><A NAME="M\d+">SYNOPSIS</A></H3>\n((?:(?!<H3>).)*)\n} $t match synopsis
	set synopsis [regsub -all {<.*?>} $synopsis {}]
	set synopsis [regsub -all {\n} $synopsis {<br />}]
		
	#
	# Get the description.
	#
	# A handful of pages have an INTRODUCTION instead of a DESCRIPTION. Most of these do
	# not document specific commands and have been omitted, but there are some exceptions.
	#
	# Regexp: find the DESCRIPTION header; starting at next line, take everything up to ". "
	# Tags are stripped (some, referencing synopsis, would be helpful to keep). Newlines
	# are replaced with <br />.
	#
	set desc {}
	regexp {<H3><A NAME="M\d+">DESCRIPTION</A></H3>\n((?:(?!\.\s).)*?\.)\s} $t match desc
	set desc [regsub -all {<.*?>} $desc {}]
	set desc [regsub -all {\n} $desc { }]
	
	#
	# Assemble the URL.
	#
	set url [format "%s%s" $url_prefix [file tail $arg]]
	
	#
	# Ignored fields. (What is the difference between description and details?)
	#
	set namespace ""
	set details ""
	set type ""
	set lang ""
	
	#
	# Output this record.
	#
	puts [format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" $name $namespace $url $desc $synopsis $details $type $lang]
}
