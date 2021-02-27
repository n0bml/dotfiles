#!/bin/bash
#set -euo pipefail
#IFS=$'\n\t'

# make a directory and change to it
function mkcd() {
    mkdir -p "$1" && cd "$1"
}

# open a file in gnome using the default application
function op() {
    gnome-open "$@" &> /dev/null
}
