if [ -n "${_MY_LOGIN_READ+x}" ]; then
  return 0 2>/dev/null || exit 0
fi
export _MY_LOGIN_READ=1
