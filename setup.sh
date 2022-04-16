#!/bin/bash
set -euo pipefail
shopt -s nullglob
IFS=$'\n\t'

sudo apt install --assume-yes build-essential vim unzip

mkdir -p "${HOME}/.local/share"
mkdir -p "${HOME}/.local/state"
mkdir -p "${HOME}/.config"

mkdir -p "${HOME}/.local/bin"
ln -s "${PWD}/local/bin/*" "${HOME}/.local/bin/"


if [[ -f "${HOME}/.bashrc" ]]; then
    cp "${HOME}/.bashrc" "${HOME}/.bashrc.dist"
fi
echo "" >> "${HOME}/.bashrc"
cat "${PWD}/setup/bashrc.bash" >> "${HOME}/.bashrc"

if [[ -f "${HOME}/.bash_aliases" ]]; then
    mv "${HOME}/.bash_alises" "${HOME}/.bash_aliases.dist"
fi
ln -s "${PWD}/bash_aliases" "${HOME}/.bash_aliases"

ln -s "${PWD}/gitconfig" "${HOME}/.gitconfig"
ln -s "${PWD}/tmux.conf" "${HOME}/.tmux.conf"
ln -s "${PWD}/flake8" "${HOME}/.flake8"
ln -s "${PWD}/.emacs.d" "${HOME}/.emacs.d"
ln -s "${PWD}/vim" "${HOME}/.vim"
