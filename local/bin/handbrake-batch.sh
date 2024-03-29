#!/bin/bash
#
# This script requires HandBrakeCLI. On macOS, at least, you need to download
# and install it separately from Handbrake.
#
# See: https://handbrake.fr/downloads2.php

if [ -z "$1" ] ; then
    TRANSCODEDIR="."
else
    TRANSCODEDIR="$1"
fi
    # Change the preset if you like (see options: "HandBrakeCLI --preset-list")
    # If you want to transcode ALL movie files, remove the -name option.
    find "$TRANSCODEDIR"/* -type f -name "*.mkv" -exec bash -c 'HandBrakeCLI -i "$1" -o "${1%\.*}".mp4 --preset="General/HQ 1080p30 Surround"' __ {} \;
