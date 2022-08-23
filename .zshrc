HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups share_history

setopt autocd extendedglob nomatch notify completealiases
unsetopt beep
bindkey -e

#zstyle :compinstall filename "$HOME.zshrc"

autoload -Uz colors && colors
#autoload -Uz select-word-style && select-word-style bash

autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
_comp_options+=(globdots)

export EDITOR='vim'

source $HOME/.config/zsh/git.zsh
source $HOME/.config/zsh/termsupport.zsh

fpath+=($HOME/.config/zsh/pure)

setopt promptsubst
autoload -Uz promptinit && promptinit

prompt pure
prompt_pure_check_cmd_exec_time() {}
#source $HOME/.config/zsh/bira-theme.zsh

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
command -v fdfind > /dev/null && alias fd='fdfind'
command -v batcat >/dev/null && alias bat='batcat'

if command -v fdfind >/dev/null; then
    export FZF_DEFAULT_COMMAND="fdfind -H -t f -E '.git/'"
elif type fd >/dev/null; then
    export FZF_DEFAULT_COMMAND="fd -H -t f -E '.git/'"
elif type rg >/dev/null; then
    export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git/*'"
fi

# FZF

source ~/.config/fzf/completion.zsh
source ~/.config/fzf/key-bindings.zsh
source ~/.config/fzf/functions.sh
source ~/.config/zsh/fzf-tab/fzf-tab.plugin.zsh

# Must be last line in config
source $HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
