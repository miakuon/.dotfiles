#!/bin/bash
layout=$(xkblayout-state print "%c")

xkblayout-state set 0

dmenu_run -i -c -l 10 -p "Execute:" # -m $1 -fn $2 -nb $3 -nf $5 -sb $5 -sf $6

xkblayout-state set $layout
