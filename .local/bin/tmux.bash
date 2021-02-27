#!/bin/bash

# Brett Terpstra 2014
# <http://brettterpstra.com>
#
# tmux wrapper
# 	tm session-name [window-name]
# Names can be partial from the beginning and first match will connect.
# If no match is found a new session will be created.
# If there's a second argument, it will be used to attach directly to a
# window in the session, or to name the first window in a new session.
tm() {
    local attach window
    if [ -n $1 ]; then
	attach=""

	tmux has-session -t $1 > /dev/null
	if [ $? -eq 0 ]; then
	    attach=$1
	    shift
	else
	    for session in `tmux ls|awk -F ":" '{ print $1 }'`;do
		if [[ $session =~ ^$1  ]]; then
		    echo "Matched session: $session"
		    attach=$session
		    shift
		    break
		fi
	    done
	fi

	if [[ $attach != "" ]]; then
	    if [ $# -eq 1 ]; then
		for win in `tmux list-windows -t $attach|sed -E 's/^[0-9]+: //'|sed -E 's/[*-].*//'`;do
		    if [[ $win =~ ^$1 ]]; then
			echo "Matched window: $window"
			window=$win
			break
		    fi
		done

		tmux attach -t $attach:$window
	    else
		tmux attach -t $attach
	    fi
	else
	    if [ $# -gt 1 ]; then
		attach=$1
		shift
		tmux new -s $attach -n $1
	    else
		echo "Attempting to create $1"
		tmux new -s $1
	    fi
	fi
    else
	tmux new
    fi
}


# print a banner for the current window name within tmux
tm-win-banner() {
    local winname sessionname
    if [[ -n $TMUX ]]; then
        sessionname=`tmux list-sessions | grep attached | awk -F: '{print $1}'`
        winname=`tmux list-windows | grep --color=never -E '^\d+: (.*)\*'| awk '{ print $2 }' | tr -d '*'`
        figlet -w `tput cols` -f graffiti "$sessionname:$winname"
    fi
}

tm-session() {
    local sessionname
    if [[ -n $TMUX ]]; then
        sessionname=`tmux list-sessions | grep attached | awk -F: '{print $1}'`
        echo -n $sessionname
    fi
}

tm-window() {
    local winname
    if [[ -n $TMUX ]]; then
        winname=`tmux list-windows | grep --color=never -E '^\d+: (.*)\*'| awk '{ print $2 }' | tr -d '*'`
        echo -n $winname
    fi
}

_tm_complete() {
    local rx
    local token=${COMP_WORDS[$COMP_CWORD]}
    local IFS=$'\t'
    local words
    if [ $COMP_CWORD -eq 2 ]; then
	words=$(tmux list-windows -t ${COMP_WORDS[1]} 2> /dev/null | awk '{print $2}' | tr -d '*-' | tr "\n" "\t")
    elif [ $COMP_CWORD -eq 1 ]; then
	words=$(tmux -q list-sessions 2> /dev/null | cut -f 1 -d ':' | tr "\n" "	")
    fi

    local nocasematchWasOff=0
    shopt nocasematch >/dev/null || nocasematchWasOff=1
    (( nocasematchWasOff )) && shopt -s nocasematch

    local w matches=()

    if [[ $token == "" ]]; then
	matches=($words)
    else
	for w in $words; do
	    rx=$(ruby -e "print '$token'.gsub(/\s+/,'').split('').join('.*')")
	    if [[ "$w" =~ $rx ]]; then
		matches+=($w)
	    fi
	done
    fi

    (( nocasematchWasOff )) && shopt -u nocasematch

    COMPREPLY=("${matches[@]}")
}

complete -F _tm_complete tm
