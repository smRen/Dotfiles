#!/usr/bin/env bash

# Disable annoying sourcing warnings, having to declare, and unused variables
# shellcheck disable=SC1090,SC2155,SC2034,SC1091

setup_emacs_vterm() {
    if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
	clear() {
	    vterm_printf "51;Evterm-clear-scrollback"
	    tput clear
	}
    fi

    # Emacs Vterm settings
    vterm_printf() {
	if [ -n "$TMUX" ] && { [ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]; }; then
	    # Tell tmux to pass the escape sequences through
	    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
	elif [ "${TERM%%-*}" = "screen" ]; then
	    # GNU screen (screen, screen-257color, screen-256color-bce)
	    printf "\eP\e]%s\007\e\\" "$1"
	else
	    printf "\e]%s\e\\" "$1"
	fi
    }

    vterm_prompt_end() {
	vterm_printf "51;A$(whoami)@$HOSTNAME:$(pwd)"
    }

    vterm_cmd() {
	local vterm_elisp
	vterm_elisp=""
	while [ $# -gt 0 ]; do
	    vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
	    shift
	done

	vterm_printf "51;E$vterm_elisp"
    }

}

setup_aliases_and_editors() {
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias diff='diff --color=auto'

    if [ -x "$(command -v emacs)" ]; then
	alias e='emacs -nw'
	alias ec='emacsclient -t'
	export EDITOR='emacsclient -t'
	export VISUAL="$EDITOR"
	export COLORTERM='truecolor'
    else
	echo "Install emacs"
    fi
}

setup_options() {
    HISTCONTROL=ignoreboth
    HISTSIZE=10000
    HISTFILESIZE=5000
    HISTFILE=~/.history

    local bash_options=("histappend" "checkwinsize" "extglob" "globstar")
    shopt -s "${bash_options[@]}"
}

setup_prompt() {
    BLACK="\[$(tput setaf 0)\]"
    YELLOW="\[$(tput setaf 3)\]"
    MAGENTA="\[$(tput setaf 5)\]"
    WHITE="\[$(tput setaf 7)\]"
    RED="\[$(tput setaf 1)\]"
    GREEN="\[$(tput setaf 2)\]"
    BLUE="\[$(tput setaf 4)\]"
    CYAN="\[$(tput setaf 6)\]"
    RESET="\[$(tput sgr0)\]"
    BOLD="\[$(tput bold)\]"
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWCOLORHINTS=1
    export PROMPT_COMMAND='__git_ps1 "[${GREEN}${BOLD}\u${BLUE}@${RED}${BOLD}\h${RESET} ${CYAN}\w${RESET}]" "\$ " " (${MAGENTA}${BOLD}git:%s${RESET}) "'
}

main() {
    # If not running interactively, don't do anything
    [[ $- != *i* ]] && return

    if ! shopt -oq posix; then
	if [ -f /usr/share/bash-completion/bash_completion ]; then
	    . /usr/share/bash-completion/bash_completion
	elif [ -f /etc/bash_completion ]; then
	    . /etc/bash_completion
	fi
    fi

    if [[ "$INSIDE_EMACS" != 'vterm' ]]; then
	cd "$HOME" || return
    fi

    setup_aliases_and_editors
    setup_options
    setup_emacs_vterm
    setup_prompt
}

main
