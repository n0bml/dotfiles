#!/bin/bash

alias realias="vi $HOME/.bash_aliases; source $HOME/.bash_aliases"

# confirm before overwriting something
alias cp="cp --interactive"
alias mv="mv --interactive"
alias rm="rm --interactive"

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

alias dsftoflac='for f in *.dsf; do echo "$f"; dsf2flac "$f" -o "${f%.dsf}".flac && rm "$f"; done'

#alias flac2alac='for i in *.flac; do echo $i; ffmpeg -i "$i" -y -v 0 -vcodec copy -acodec alac "${i%.flac}".m4a && rm "$i"; done'
alias flac2aac='for i in *.flac; do echo $i; ffmpeg -i "$i" -y -v 0 -vcodec copy -acodec aac "${i%.flac}".m4a && rm "$i"; done'
alias flac2mp3='for i in *.flac; do echo $i; ffmpeg -i "$i" -y -codec:a libmp3lame -q:a 0 -map_metadata 0 -id3v2_version 3 -write_id3v1 1 "${i%.flac}.mp3" && rm -f "$i"; done'

alias webp2png='for f in *.webp; do echo "$f"; dwebp "$f" -o "${f%.webp}".png && rm "$f"; done'

alias dirty='watch --differences grep --regexp=Dirty: --regexp=Writeback: /proc/meminfo'
alias df='df --human-readable'
alias ducks='du --total --block-size=1K --summarize --human-readable'
alias free='free --human'
alias lynx='lynx -cfg=~/.config/lynx/lynx.cfg' # -lss=~/.config/lynx/lynx.lss
alias psmem='ps auxf | sort --numeric-sort --reverse --key=4'
alias pscpu='ps auxf | sort --numeric-sort --reverse --key=3'

alias envup="export \$(echo \$(grep --invert-match '^#' .env | xargs) | envsubst)"
alias ce="python -m venv .venv"
alias ae='deactivate &> /dev/null; source ./.venv/bin/activate'
alias de='deactivate'

alias zcat='gzip --decompress --stdout'

alias lstty="ls -l /dev/serial/by-id"
alias lsrig="rigctl --list"
alias lssnd="aplay --list-devices && arecord --list-devices"
