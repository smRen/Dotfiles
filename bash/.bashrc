#!/usr/bin/env bash

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
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
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

setup_fzf() {
  # Fancy history search
  if [ "$HOSTNAME" = "archlocal.debianthinkpad" ]; then
    local FZF_KEYBINDING_BASH="/usr/share/fzf/key-bindings.bash"
    local FZF_COMPLETION_BASH="/usr/share/fzf/completion.bash"
  elif [ "$HOSTNAME" = "debianthinkpad" ]; then
    local FZF_KEYBINDING_BASH="/usr/share/doc/fzf/examples/completion.bash"
    local FZF_COMPLETION_BASH="/usr/share/doc/fzf/examples/key-bindings.bash"
  fi

  [ -r "$FZF_KEYBINDING_BASH" ] && . "$FZF_KEYBINDING_BASH"

  [ -r "$FZF_COMPLETION_BASH" ] && . "$FZF_COMPLETION_BASH"
}

setup_git() {
  local GIT_COMPLETION_BASH="/usr/share/git/completion/git-completion.bash"
  [ -r "$GIT_COMPLETION_BASH" ] && . "$GIT_COMPLETION_BASH"
  local GIT_PROMPT="/usr/share/git/completion/git-prompt.sh"
  [ -r "$GIT_PROMPT" ] && . "$GIT_PROMPT"
}

setup_aliases() {
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  [ "$TERM" = "xterm-kitty" ] && alias ssh="kitty +kitten ssh"
  alias e='TERM=xterm-direct emacs -nw'
  alias ec='TERM=xterm-direct emacsclient -nw'
}

setup_options() {
  HISTCONTROL=ignoreboth
  HISTSIZE=10000
  HISTFILESIZE=5000
  HISTFILE=~/.history

  local OPTIONS=("histappend" "checkwinsize" "extglob" "globstar")
  shopt -s "${OPTIONS[@]}"
}

setup_prompt() {
  local BLACK=$(tput setaf 0)
  local RED=$(tput setaf 1)
  local GREEN=$(tput setaf 2)
  local YELLOW=$(tput setaf 3)
  local BLUE=$(tput setaf 4)
  local MAGENTA=$(tput setaf 5)
  local CYAN=$(tput setaf 6)
  local WHITE=$(tput setaf 7)
  local RESET=$(tput sgr0)
  local BOLD=$(tput bold)
  export PS1="[${GREEN}${BOLD}\u${BLUE}@${RED}${BOLD}\h${RESET} ${CYAN}\W ${RESET}$(__git_ps1 ' (%s)')] \$ "
}

main() {
  # If not running interactively, don't do anything
  [[ $- != *i* ]] && return

  if [ "$HOSTNAME" = "archlocal.debianthinkpad" ]; then
    cd "$HOME" || return
  fi

  setup_aliases
  setup_options
  setup_fzf
  setup_emacs_vterm
  setup_git
  setup_prompt
}

main
