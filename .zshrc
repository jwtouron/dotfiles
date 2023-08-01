# History
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups share_history histverify
alias priv=' ZSH_PRIV=1 zsh'
[ -n "$ZSH_PRIV" ] && unset HISTFILE
# fc -R $DEF_HISTFILE

setopt autocd extendedglob nomatch notify completealiases
unsetopt beep
bindkey -e

#zstyle :compinstall filename "$HOME.zshrc"

autoload -Uz colors && colors
autoload -Uz select-word-style && select-word-style bash

autoload -Uz compinit && compinit
# zstyle ':completion:*' menu select
# _comp_options+=(globdots)

# zmodload zsh/complist
# bindkey -M menuselect 'h' vi-backward-char
# bindkey -M menuselect 'k' vi-up-line-or-history
# bindkey -M menuselect 'j' vi-down-line-or-history
# bindkey -M menuselect 'l' vi-forward-char
# bindkey '^I' menu-complete

source $HOME/.config/zsh/git.zsh
# source $HOME/.config/zsh/termsupport.zsh

fpath+=($HOME/.config/zsh/pure)
setopt promptsubst
autoload -Uz promptinit && promptinit

prompt pure
prompt_pure_check_cmd_exec_time() {}
[ -n "$ZSH_PRIV" ] && export PS1="%F{yellow}[PRIVATE] $PS1"
#source $HOME/.config/zsh/bira-theme.zsh

# FZF
source ~/.config/fzf/completion.zsh
source ~/.config/fzf/key-bindings.zsh
source ~/.config/zsh/fzf-tab/fzf-tab.plugin.zsh
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

# Functionality common to bash and zsh
source ~/.config/shell/rc

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Must be last line in config
source $HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
