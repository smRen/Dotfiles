# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/var/lib/flatpak/exports/bin" ]; then
    PATH="/var/lib/flatpak/exports/bin:$PATH"
fi

if [ -d "$HOME/.local/share/flatpak/exports/bin" ]; then
    PATH="$HOME/.local/share/flatpak/exports/bin:$PATH"
fi

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    export MOZ_ENABLE_WAYLAND=1
fi

# # set PATH to include local bin
# PATH="/usr/local/bin/:$PATH"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	      . "$HOME/.bashrc"
    fi
fi

# pyenv bin
if [ -d "$HOME/.pyenv/bin" ] ; then
    PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv init --path)"
fi

xhost +si:localuser:$USER
