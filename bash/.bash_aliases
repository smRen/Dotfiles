# Konsole terminal specific settings
if [[ $HOSTNAME == "archserver" || $HOSTNAME == "archsp4" ]]; then
    konsole_str="TERM=konsole-direct "
else
    konsole_str=""
fi
alias e="${konsole_str}emacs -nw"
alias ec="${konsole_str}emacsclient -nw"
alias build_emacs="sudo docker build -t emacs . && sudo docker run -it --mount type=bind,source=$HOME,target=$HOME emacs"
