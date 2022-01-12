# Emacs
alias e="emacs -nw"
alias ec="emacsclient -nw"
alias build_emacs="sudo docker build -t emacs . && sudo docker run -it --mount type=bind,source=$HOME,target=$HOME emacs"
