#!/bin/bash

alias realias="vi $HOME/.bash_aliases; source $HOME/.bash_aliases"

# confirm before overwriting something
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

alias ..="cd .."
alias ...="cd ../.."

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

alias ls='ls --color=auto --group-directories-first --classify'
alias la='ls --almost-all'
alias ll='ls --color=auto -l'
alias lla='ls --color=auto --almost-all -l'
alias l1='ls --color=auto -1'

alias damnit='vim $(git grep --files-with-matches --extended-regexp "^<<<<<")'

alias webp2png='for f in *.webp; do echo "$f"; dwebp "$f" -o "${f%.webp}".png && rm "$f"; done'

alias df='df -h'
alias free='free -h'
alias lynx='lynx -cfg=~/.config/lynx/lynx.cfg' # -lss=~/.config/lynx/lynx.lss
alias ducks='du -cksh'
alias psmem='ps auxf | sort -nr -k 4'
alias pscpu='ps auxf | sort -nr -k 3'
alias dirty='watch -d grep -e Dirty: -e Writeback: /proc/meminfo'

alias envup="export \$(echo \$(grep -v '^#' .env | xargs) | envsubst)"
alias ce="python -m venv .venv"
alias ae='deactivate &> /dev/null; source ./.venv/bin/activate'
alias de='deactivate'

alias zcat='gzip --decompress --stdout'
