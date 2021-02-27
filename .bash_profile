#!/bin/bash

# Read by login shells, but the distinction is unimportant, so just read the
# real .bashrc here.

source "$HOME/.bashrc"

if [[ -f "$HOME/.bash_profile.local" ]]; then
    echo "*** Not reading .bash_profile.local: rename to .bashrc.local"
fi

export BML_BASH_PROFILE=1
