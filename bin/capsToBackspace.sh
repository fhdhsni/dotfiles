#!/bin/bash
xmodmap -e "clear lock" #disable caps lock switch
xmodmap -e "keysym Caps_Lock = BackSpace" #set caps_lock as backspace
