#!/bin/bash
setxkbmap -option grp:switch,grp:menu_toggle us,fr
setxkbmap -option compose:rctrl
pkill xcape
. $HOME/bin/ctrl.sh
. $HOME/bin/capsToBackspace.sh
echo "us,fr"
