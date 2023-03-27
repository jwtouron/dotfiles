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

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char

source $HOME/.config/zsh/git.zsh
# source $HOME/.config/zsh/termsupport.zsh

fpath+=($HOME/.config/zsh/pure)

setopt promptsubst
autoload -Uz promptinit && promptinit

prompt pure
prompt_pure_check_cmd_exec_time() {}
#source $HOME/.config/zsh/bira-theme.zsh

# FZF
source ~/.config/fzf/completion.zsh
source ~/.config/fzf/key-bindings.zsh
# source ~/.config/zsh/fzf-tab/fzf-tab.plugin.zsh

# Functionality common to bash and zsh
source ~/.config/shell/rc

# Must be last line in config
source $HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
