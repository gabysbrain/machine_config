#!/bin/sh
setxkbmap -query | grep layout | sed 's/layout: *\(.*\)/\1/'
