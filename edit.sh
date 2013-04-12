#!/usr/bin/env bash

export LINES=`tput lines`
export COLUMNS=`tput cols`

stty -icanon min 1
stty -echo
tput civis
lein run "$@"
tput cnorm
stty echo
