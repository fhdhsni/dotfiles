#!/bin/bash
setxkbmap -option grp:switch,grp:menu_toggle us,ir
setxkbmap -option compose:rctrl
pkill xcape
. $HOME/bin/ctrl.sh
. $HOME/bin/capsToBackspace.sh
echo "us,ir"
