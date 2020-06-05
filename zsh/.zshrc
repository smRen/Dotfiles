# Needed settings to allow Emacs Tramp mode
if [[ "$TERM" == "dumb" ]]; then
   unsetopt zle
   unsetopt prompt_cr
   unsetopt prompt_subst
   unfunction precmd
   unfunction preexec
   PS1='$ '
   return
fi

# Emacs readline
bindkey -e

source ~/.zplug/init.zsh

zplug "dracula/zsh", as:theme
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"

zplug load

# ZSH history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory


path+=('/home/smakey18/.pyenv/bin')
path+=('/home/smakey18/Applications/')
export PATH
export PIPENV_VENV_IN_PROJECT=1
export FZF_DEFAULT_COMMAND='rg --files --hidden'
#source /usr/share/doc/fzf/examples/key-bindings.zsh
source /usr/share/fzf/fzf-extras.zsh
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
# source /usr/share/nvm/init-nvm.sh
# source /usr/share/fzf/fzf-extras.zsh
# source /usr/share/fzf/key-bindings.zsh
# source /usr/share/fzf/completion.zsh
# source /usr/share/doc/fzf/examples/key-bindings.zsh # Debian
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then
  source "${VIRTUAL_ENV}/bin/activate"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# For Vterm
function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
