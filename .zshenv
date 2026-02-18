: "${XDG_CONFIG_HOME:=$HOME/.config}"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

[ -r "$XDG_CONFIG_HOME/shell/env.sh" ] && . "$XDG_CONFIG_HOME/shell/env.sh"
