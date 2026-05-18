if [ -n "${_MY_INTERACTIVE_READ+x}" ]; then
  return 0 2>/dev/null || exit 0
fi
_MY_INTERACTIVE_READ=1

alias wl='wc -l'

if command -v lsd >/dev/null; then
    alias  ls='lsd --icon never'
    alias  la='lsd --icon never -A'
    alias  ll='lsd --icon never -Alh'
    alias ltr='lsd --icon never -Alhtr'
else
    alias  ls='ls --color=auto'
    alias  la='ls --color=auto -A'
    alias  ll='ls --color=auto -Alh'
    alias ltr='ls --color=auto -Alhtr'
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
