[[ $- != *i* ]] && return
[[ "$(whoami)" = "root" ]] && return

[[ -z "$FUNCNEST" ]] && export FUNCNEST=100          # limits recursive functions, see 'man bash'

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s histappend

shopt -s checkwinsize
shopt -s globstar

## Use the up and down arrow keys for finding a command in history
## (you can write some initial letters of the command first).
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

export EDITOR=vim

alias wl='wc -l'

alias ls='ls --color=auto'
alias ll='ls -lavF'   # show long listing of all except ".."
alias la='ls -AF'
alias l='ls -lavF'   # show long listing but no hidden dotfiles except "."
alias lt='ls -lAFt'
alias ltr='ls -lAFtr'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias config='/usr/bin/git --git-dir=$HOME/.config/.dotfiles/ --work-tree=$HOME'
alias config-sync='GIT_DIR=$HOME/.config/.dotfiles GIT_WORK_TREE=$HOME git-sync'
command -v fdfind >/dev/null && alias fd='fdfind'
command -v batcat >/dev/null && alias bat='batcat'

# FZF

if command -v fdfind >/dev/null; then
    export FZF_DEFAULT_COMMAND="fdfind -H -t f -E '.git/'"
elif command -v fd >/dev/null; then
    export FZF_DEFAULT_COMMAND="fd -H -t f -E '.git/'"
elif command -v rg >/dev/null; then
    export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git/*'"
fi

source ~/.config/fzf/functions.sh
source ~/.config/fzf/key-bindings.bash

command -v starship >/dev/null && eval "$(starship init bash)"
