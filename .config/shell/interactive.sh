if [ -n "${_MY_INTERACTIVE_READ+x}" ]; then
  return 0 2>/dev/null || exit 0
fi
_MY_INTERACTIVE_READ=1

alias wl='wc -l'

if command -v lsd >/dev/null; then
    alias ls='lsd --icon never -F'
    alias la='lsd --icon never -AFh'
    alias ll='lsd --icon never -AFhl'
    alias ltr='lsd --icon never -AFhltr'
else
    alias ls='ls -F'
    alias la='ls -AFh'
    alias ll='ls -AFhl'
    alias ltr='ls -AFhltr'
fi

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias config='git --git-dir=$HOME/.local/share/dotfiles/ --work-tree=$HOME'
alias config-sync='GIT_DIR=$HOME/.local/share/dotfiles GIT_WORK_TREE=$HOME git-sync'

source ~/.config/shell/man

if command -v trash-put >/dev/null; then
    alias rm='echo "This is not the command you are looking for."; false'
fi
