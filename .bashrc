# vim: ft=bash

[[ $- != *i* ]] && return
[[ "$(whoami)" = "root" ]] && return

[[ -z "$FUNCNEST" ]] && export FUNCNEST=100          # limits recursive functions, see 'man bash'

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=1000
shopt -s histappend

shopt -s checkwinsize
shopt -s globstar

## Use the up and down arrow keys for finding a command in history
## (you can write some initial letters of the command first).
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

# Private Mode
alias priv='BASH_PRIV=1 bash'
[ -n "$BASH_PRIV" ] && unset HISTFILE

# FZF
source "$HOME/.config/fzf/key-bindings.bash"
source "$HOME/.config/fzf/completion.bash"
_fzf_setup_completion path ag git kubectl
_fzf_setup_completion dir tree
_fzf_setup_completion path config

# Functionality common to bash and zsh
source "$HOME/.config/shell/rc"

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"

# Prompt
PROMPT_COMMAND=__prompt_command
__prompt_command() {
    local last_error="$?"
    local blue="\e[0;34m"
    local green="\e[0;32m"
    local red="\e[0;31m"
    local yellow="\e[0;33m"
    local nc="\e[0m"

    PS1="\n${blue}\w${nc}"

    # Git
    local git_status
    git_status="$(git status 2>&1)"
    if echo "$git_status" | grep -q -v "fatal: not a git repository" -; then
        local branch
        branch=$(echo "$git_status" |  cut -d ' ' -f 3 | head -n1)
        PS1+=" ${yellow} $branch ${nc}"
        echo "$git_status" | grep -q "nothing to commit, working tree clean" - && \
            PS1+="${green}✓${nc}" || \
            PS1+="${red}✗${nc}"
    fi

    PS1+="\n"
    [ -n "$BASH_PRIV" ] && PS1+="${yellow}[PRIVATE]${nc} "
    [[ "$last_error" -eq 0 ]] && PS1+="${green}" || PS1+="${red}"
    PS1+="❯${nc} "
}
# command -v starship ❯/dev/null && eval "$(starship init bash)"
