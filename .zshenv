: "${XDG_CONFIG_HOME:=$HOME/.config}"
: "${XDG_STATE_HOME:=$HOME/.local/state}"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

if [[ -o interactive ]]; then
    export HISTFILE="$XDG_STATE_HOME/zsh/history"
    mkdir -p -- "${HISTFILE:h}"
fi

[ -r "$XDG_CONFIG_HOME/shell/env.sh" ] && . "$XDG_CONFIG_HOME/shell/env.sh" 
