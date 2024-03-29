#!/usr/bin/bash

export N0BML_BASH_PROFILE=1

# if running bash
if [[ -n "${BASH_VERSION}" ]]; then
    # include .bashrc if it exists
    if [[ -f "${HOME}/.bashrc" ]]; then
        source "${HOME}/.bashrc"
    else
        # set PATH so it includes user's private bin if it exists
        if [[ -d "${HOME}/.local/bin" ]]; then
            PATH="${HOME}/.local/bin:${PATH}"
        fi
    fi
fi
