#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# first remove the existing bogofilter DB, if it exists
if [[ -f "${HOME}/.bogofilter/wordlist.db" ]]; then
    rm "${HOME}/.bogofilter/wordlist.db"
fi

echo train with known spam...
bogofilter -M -v -s -I "${HOME}/Mail/yyy-spam"

echo train with known ham...
for HAM in `ls "${HOME}/Mail/"`; do
    if [[ ! "${HAM}" == "yyy-spam" ]]; then
        bogofilter -M -v -n -I "${HOME}/Mail/${HAM}"
    fi
done
