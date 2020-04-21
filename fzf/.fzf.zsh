# Setup fzf
# ---------
if [[ ! "$PATH" == */home/smakey18/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/smakey18/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/smakey18/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/smakey18/.fzf/shell/key-bindings.zsh"
