# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load completions and git prompt
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    else
	echo "Install bash completion"
    fi

    if [ -f /usr/share/git/completion/git-prompt.sh ]; then
	. /usr/share/git/completion/git-prompt.sh
    fi
fi

if [[ "$INSIDE_EMACS" != 'vterm' ]]; then
    cd "$HOME" || return
fi

# Customize prompt
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

# History settings
HISTCONTROL=ignoreboth
HISTSIZE=10000
HISTFILESIZE=5000
HISTFILE=~/.history
shopt -s "histappend" "checkwinsize" "extglob" "globstar"

# Aliases
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

# For Emacs Vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    clear() {
	vterm_printf "51;Evterm-clear-scrollback"
	tput clear
    }
fi

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
