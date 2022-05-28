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

setup_fzf() {
  # Fancy history search
  if [[ "$HOSTNAME" == *"arch"* ]]; then
    local fzf_keybinding_bash="/usr/share/fzf/key-bindings.bash"
    local fzf_completion_bash="/usr/share/fzf/completion.bash"
  else
    local fzf_keybinding_bash="/usr/share/doc/fzf/examples/completion.bash"
    local fzf_completion_bash="/usr/share/doc/fzf/examples/key-bindings.bash"
  fi

  [[ -r "$fzf_keybinding_bash" ]] && . "$fzf_keybinding_bash"

  [[ -r "$fzf_completion_bash" ]] && . "$fzf_completion_bash"
}

setup_git() {
  local git_completion_bash="/usr/share/git/completion/git-completion.bash"
  [[ -r "$git_completion_bash" ]] && . "$git_completion_bash"
  local git_prompt="$HOME/Scripts/git-prompt.sh"
  [[ -r "$git_prompt" ]] && . "$git_prompt"
}

setup_aliases_and_editors() {
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  [[ "$TERM" == 'xterm-kitty' ]] && alias ssh='kitty +kitten ssh'

  # Flatpak setup for editors
  alias e='TERM=xterm-direct emacs -nw'
  alias ec='TERM=xterm-direct emacsclient -t'
  alias vim='flatpak run --env=TERM=xterm-256color org.vim.Vim'
  export EDITOR='TERM=xterm-direct emacsclient -t'
  export VISUAL="$EDITOR"
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


  if [[ "$HOSTNAME" =~ archapps.* ]] && [[ "$INSIDE_EMACS" != 'vterm' ]]; then
    cd "$HOME" || return
  fi

  setup_aliases_and_editors
  setup_options
  setup_fzf
  setup_emacs_vterm
  setup_git
  setup_prompt
}

main
