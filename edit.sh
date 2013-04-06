#!/usr/bin/env bash

export LINES=`tput lines`
export COLUMNS=`tput cols`

stty -icanon min 1
lein run
