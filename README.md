# Connect4Lisp
A connect four clone written in LISP
Written for CS 495 - Artificial Intelligence as a final project

# PREREQUISITS

Connect4Lisp was written for clisp but would probably work with
other versions of LISP as well. GNU clisp is available on *NIX systems,
Mac, Windows, and other platforms as well; It can be found here:

	http://www.clisp.org
	
To run the GUI version of Connect4Lisp, you will need Tcl/Tk
installed as well as clisp. In this directory a windows installer
is included for ActiveState's windows port of Tcl/Tk; their website
can be found here:

	https://www.activestate.com/activetcl
	
Tcl/Tk is installable on Linux systems; and has been tested on Arch.

To install Tcl/Tk on arch:

	sudo pacman -S tcl tk

# RUNNING
From either a terminal (*NIX) or command prompt (Windows), run

	clisp connect4.lisp

for the command line version, or
	
	clisp connect4-withgui.lisp

for the gui version

