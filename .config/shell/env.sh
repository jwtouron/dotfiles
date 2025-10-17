if [ -n "${_MY_ENV_READ+x}" ]; then
  return 0 2>/dev/null || exit 0
fi
export _MY_ENV_READ=1

__command() {
    command -v "$1" >/dev/null
}

: "${XDG_CONFIG_HOME:=$HOME/.config}"

PATH="$HOME/.local/bin:$HOME/bin:$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.nimble/bin:$HOME/.cargo/bin:$PATH"

for browser in brave brave-browser firefox chromium chrome google-chrome; do
    if __command "$browser"; then
        export BROWSER="$browser"
        break
    fi
done

for editor in nvim vim vi nano; do
    if __command "$editor"; then
        export EDITOR="$editor"
        break
    fi
done

for terminal in kitty wezterm alacritty st xterm; do
    if __command "$terminal"; then
        export TERMINAL="$terminal"
        break
    fi
done

if __command fdfind; then
    export FZF_DEFAULT_COMMAND="fdfind -H -t f -E '.git/'"
elif __command fd; then
    export FZF_DEFAULT_COMMAND="fd -H -t f -E '.git/'"
elif __command rg; then
    export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git/*'"
fi
