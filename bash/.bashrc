#!/usr/bin/env bash

setup_emacs_vterm() {
  if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear() {
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
  local FZF_KEYBINDING_BASH="/usr/share/fzf/key-bindings.bash"
  [ -r "$FZF_KEYBINDING_BASH" ] && . "$FZF_KEYBINDING_BASH"

  local FZF_COMPLETION_BASH="/usr/share/fzf/completion.bash"
  [ -r "$FZF_COMPLETION_BASH" ] && . "$FZF_COMPLETION_BASH"
}

setup_powerline() {
  # Fancy status
  local POWERLINE_DAEMON="/usr/bin/powerline-daemon"
  local POWERLINE_BASH_BINDING="/usr/share/powerline/bindings/bash/powerline.sh"

  if [ -x "$POWERLINE_DAEMON" ] && [ "$INSIDE_EMACS" != 'vterm' ]; then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
    . "$POWERLINE_BASH_BINDING"
  fi
}

setup_git() {
  local GIT_COMPLETION_BASH="usr/share/git/completion/git-completion.bash"
  [ -r "$GIT_COMPLETION_BASH" ] && . "$GIT_COMPLETION_BASH"
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
  HISTFILESIZE=2000

  local OPTIONS=("histappend" "checkwinsize" "extglob" "globstar")
  shopt -s "${OPTIONS[@]}"
}

main() {
  # If not running interactively, don't do anything
  [[ $- != *i* ]] && return

  if [ "$HOSTNAME" = "archlocal.debianthinkpad" ]; then
    cd "$HOME" || return
  fi

  if [ "$HOSTNAME" = "debianthinkpad" ] || [ "$HOSTNAME" = "archlocal.debianthinkpad" ]; then
    setup_aliases
    setup_options
    setup_powerline
    setup_fzf
    setup_emacs_vterm
  fi
}

main
