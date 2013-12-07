# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd beep extendedglob nomatch notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/john/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt correctall

# Autocompletion colors
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

autoload -U promptinit
promptinit

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
  colors
fi

export PROMPT="%(?.${fg_bold[green]}✔ ${fg_bold[$reset_color]}.${fg_bold[red]}✗ %? ${fg_bold[$reset_color]})%{${fg_bold[green]}%}%n@%m%{$reset_color%}%# "
export RPROMPT="%{${fg_bold[cyan]}%}%~%{$reset_color%}"

# Change title of MinTTY to current dir
function settitle() {
    echo -ne "\033]2;"$1"\007"
}
function chpwd() {
    settitle $(cygpath -m `pwd`)
}
settitle $(cygpath -m `pwd`)

alias ls='ls --color=auto'

alias gitk='cmd /c gitk'
alias emacs='cmd /c emacs'
