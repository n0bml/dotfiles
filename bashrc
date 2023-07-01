# everybody doesn't need to read my diary
umask 027

# I want UTF-8 dammit!
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set the Free Desktop Specification environment variables, if not already set
export XDG_DATA_HOME=${XDG_DATA_HOME:="${HOME}/.local/share"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="${HOME}/.config"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="${HOME}/.cache"}

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# setup some options for various programs
export BC_ENV_ARGS="--mathlib"

# only set the term capabilities to support 256 color if we're not in tmux
if [[ -z "${TMUX}" ]]; then
    export TERM=xterm-256color
fi

# include some useful bash functions
BASH_FILES="${HOME}/.dotfiles/bash/*.bash"
for F in $BASH_FILES; do
    source "$F"
done
unset BASH_FILES

# pyenv
if [[ -d "${HOME}/.pyenv" ]]; then
    export PYENV_ROOT="${HOME}/.pyenv"
    export PATH="${PYENV_ROOT}/bin:${PATH}"
    eval "$(pyenv init -)"

    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    eval "$(pyenv virtualenv-init -)"
fi

# add startup for interactive Python REPLs.
if [[ -f "${HOME}/.dotfiles/startup.py" ]]; then
    export PYTHONSTARTUP="${HOME}/.dotfiles/startup.py"
fi

# initialize ssh and add my keys
export SSH_ENV="$HOME/.ssh/environment"

function start_ssh_agent {
    echo "Initializing new SSH agent..."
    /usr/bin/ssh-agent -s | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    source "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

if [[ -f "${SSH_ENV}" ]]; then
    source "${SSH_ENV}" > /dev/null
    ps -ef | grep ${SSH_AGENT_PID} | grep "ssh-agent -s$" >/dev/null || {
        start_ssh_agent;
    }
else
    start_ssh_agent;
fi

# add git bash prompt, if it exists
if [[ -d "${HOME}/.bash-git-prompt" ]]; then
    export GIT_PROMPT_ONLY_IN_REPO=1
    export GIT_PROMPT_END_USER=" \n$WHITE\u@\h$ResetColor \$ "
    export GIT_PROMPT_END_ROOT=" \n$WHITE\u@\h$ResetColor # "
    source "${HOME}/.bash-git-prompt/gitprompt.sh"
fi

export PGHOST="asustor.glitterhaus.net"

export VIMCONFIG="${HOME}/.vim"
export VIMDATA="${HOME}/.vim"

# where the hell am I? (ISO 6709 format)
# TODO write program to update when GPS device is connected
export LOCATION=+47.305464-122.215806/
export LOCATION_NAME="Auburn, WA"
export LOCATION_GRID="CN87vh"

# read a .local file if it exists
if [[ -f "${HOME}/.bashrc.local" ]]; then
    source "${HOME}/.bashrc.local"
fi

if [[ -f "${HOME}/.local/share/games/fortunes/n0bml-fortunes" ]]; then
    echo " "
    fortune ~/.local/share/games/fortunes/n0bml-fortunes
fi

export N0BML_BASHRC=1
