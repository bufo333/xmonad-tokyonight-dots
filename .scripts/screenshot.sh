#!/usr/bin/env bash

time=$(date +%Y-%m-%d-%H-%M-%S)
geometry=$(xrandr | head -n1 | cut -d',' -f2 | tr -d '[:blank:],current')
dir="$HOME/Pictures/Screenshots"
file="Screenshot_${time}_${geometry}.png"

maim -f png $dir/$file
