#!/usr/local/bin/fish
#
# launch.sh - xmonad launch script
#
~/.scripts/rotate.sh &
picom -b  &                                                 # start compositor
spotifyd &
pulseaudio &
parcellite &
