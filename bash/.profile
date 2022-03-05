# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# set PATH to include local bin
PATH="/usr/local/bin/:$PATH"

# pyenv bin
if [ -d "$HOME/.pyenv/bin" ] ; then
    PATH="$HOME/.pyenv/bin:$PATH"
fi
eval "$(pyenv init -)"
eval "$(pyenv init --path)"

# Emacs aliases
alias e="emacs -nw"
alias ec="emacsclient -nw"
export EDITOR="emacsclient -nw"
export VISUAL=$EDITOR

if [[ "$HOSTNAME" = kubuntu* ]]; then
    alias tmux="/snap/bin/tmux-non-dead.tmux"
fi
