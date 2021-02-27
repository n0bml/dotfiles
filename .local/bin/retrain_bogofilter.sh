#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

echo retrain with known spam...
bogofilter -M -v -Ns -I "${HOME}/Mail/yyy-spam"

echo retrain with known ham...
for HAM in `ls "${HOME}/Mail/"`; do
    if [[ ! "${HAM}" == "yyy-spam" ]]; then
        bogofilter -M -v -Sn -I "${HOME}/Mail/${HAM}"
    fi
done
