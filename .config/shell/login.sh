if [ -n "${_MY_LOGIN_READ+x}" ]; then
  return 0 2>/dev/null || exit 0
fi
export _MY_LOGIN_READ=1

[ -x /home/linuxbrew/.linuxbrew/bin/brew ] && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
