#!/bin/sh

i3status | while :
do
    read line
    lang=`/home/farhad/bin/get_kb_layout.sh`
    echo "$lang | $line" || exit 1
done
