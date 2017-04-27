Connect4Lisp
A connect four clone written in LISP
Written for CS 495 - Artificial Intelligence as a final project
Author: William blair

# PREREQUISITS

Connect4Lisp was written for ANSI clisp. GNU clisp is available on *NIX 
systems, Mac, Windows, and other platforms as well; It can be 
found here:

	http://www.clisp.org
	
In addition, you will need the Tcl/Tk GUI toolkit
installed as well as clisp. In this directory a windows installer
is included for ActiveState's windows port of Tcl/Tk; their website
can be found here:

	https://www.activestate.com/activetcl
	
Tcl/Tk is installable on Linux systems; and has been tested on Arch.

To install Tcl/Tk on arch:

	sudo pacman -S tcl tk

Debian:

	sudo apt-get install tcl tk

# RUNNING
From either a terminal (*NIX) or command prompt (Windows), run

	clisp connect4-withgui.lisp

# STRATEGIES
The computer opponent follows a set of rules while playing, in the
following order:

1) If there is a winning move for itself, take it
2) If the opponent (player 1) has a winning move, block it
3) If player 1 has two pieces somewhere in the middle, with
   the left and right of them open, place to the left - this
   prevents player 1 from having 3 in a row with an option
   to win on the left or the right side, which is unblockable for
   the computer
3) Place within the center 3 columns if they aren't full
	- assuming this move doesn't give a winning move to player 1
4) Place within the middle 5 columns if they aren't full
	- assuming this move doesn't give a winning move to player 1
5) Place randomly

One strategy to beat the computer would be to take advantage of
the computer's priority to block a winning move by having it so
that blocking your first winning move creates another opening for
yourself. Example (0-open, 1-player 1, 2-computer):

board status 1:
    1 1 0 1
    1 1 0 1
  
board status 2:
    1 1 0 1
    1 1 2 1

(player 2 blocks the first winning move)

board status 3:
    1 1 1 1
    1 1 2 1
    
(you play on top of the computer's last move - you win!)
