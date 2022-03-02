#!/bin/sh

if [ $HOSTNAME = "kubuntu" ]; then
    /usr/bin/setxkbmap -option 'caps:ctrl_modifier' && xcape -e 'Caps_Lock=Escape' -t 100
    /usr/bin/imwheel -b "4 5"
    /snap/bin/emacs -nw --daemon
else
    /usr/bin/emacs -nw --daemon
fi
