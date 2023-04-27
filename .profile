PATH="$HOME/.local/bin:$HOME/bin:$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.nimble/bin:$PATH"

[ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

__command() {
    command -v "$1" >/dev/null && true || false
}

for browser in firefox brave brave-browser chromium chrome google-chrome; do
    if __command "$browser"; then
        BROWSER="$browser"
        break
    fi
done
export BROWSER

for editor in nvim vim vi nano; do
    if __command "$editor"; then
        export EDITOR="$editor"
        break
    fi
done

for terminal in st wezterm alacritty xterm; do
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

 # vim: ft=bash:
