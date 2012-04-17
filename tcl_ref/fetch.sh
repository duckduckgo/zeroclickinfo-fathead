#!/bin/bash

#
# Retrieves Tcl/Tk 8.5 Manual pages from <http://www.tcl.tk/man/tcl8.5/>.
# Rather than scrape the contents pages for links to download, we list specific commands.
# Some manual pages do not document specific commands and have been omitted below. 
#

#
# Tcl/Tk Applications
# <http://www.tcl.tk/man/tcl8.5/UserCmd/contents.htm>
#
dir="UserCmd"
mkdir -p download/${dir}
for cmd in tclsh wish
do
	curl "http://www.tcl.tk/man/tcl8.5/${dir}/${cmd}.htm" \
			--output "download/${dir}/${cmd}.htm"
done

#
# Tcl Commands
# <http://www.tcl.tk/man/tcl8.5/TclCmd/contents.htm>
#
dir="TclCmd"
mkdir -p download/${dir}
for cmd in after append apply array bgerror binary break catch cd chan clock close \
		concat continue dde dict encoding eof error eval exec exit expr fblocked \
		fconfigure fcopy file fileevent flush for foreach format gets glob \
		global history http if incr info interp join lappend lassign lindex \
		linsert list llength load lrange lrepeat lreplace lreverse lsearch lset lsort \
		mathfunc mathop memory msgcat namespace open package packagens pid pkgMkIndex \
		platform platform_shell proc puts pwd read refchan regexp registry regsub rename \
		return safe scan seek set socket source split string subst switch tcltest \
		tclvars tell time tm trace unknown unload unset update uplevel upvar variable \
		vwait while
do
	curl "http://www.tcl.tk/man/tcl8.5/${dir}/${cmd}.htm" \
			--output "download/${dir}/${cmd}.htm"
done

#
# Tk Commands
# <http://www.tcl.tk/man/tcl8.5/TkCmd/contents.htm>
#
dir="TkCmd"
mkdir -p download/${dir}
for cmd in bell bind bindtags bitmap button canvas checkbutton chooseColor \
		chooseDirectory clipboard colors console cursors destroy dialog entry event \
		focus focusNext font frame getOpenFile grab grid image keysyms label labelframe \
		listbox loadTk lower menu menubutton message messageBox option optionMenu \
		options pack palette panedwindow photo place popup radiobutton raise scale \
		scrollbar selection send spinbox text tk tkerror tkvars tkwait toplevel \
		ttk_button ttk_checkbutton ttk_combobox ttk_entry ttk_frame ttk_image \
		ttk_label ttk_labelframe ttk_menubutton ttk_notebook ttk_panedwindow \
		ttk_progressbar ttk_radiobutton ttk_scale ttk_scrollbar ttk_separator \
		ttk_sizegrip ttk_spinbox ttk_style ttk_treeview ttk_vsapi winfo wm
do 
	curl "http://www.tcl.tk/man/tcl8.5/${dir}/${cmd}.htm" \
			--output "download/${dir}/${cmd}.htm"
done
