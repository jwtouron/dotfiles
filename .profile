PATH="$HOME/.local/bin:$HOME/bin:$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.nimble/bin:$HOME/.cargo/bin:$PATH"

[ -x /home/linuxbrew/.linuxbrew/bin/brew ] && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

__command() {
    command -v "$1" >/dev/null
}

for browser in brave brave-browser firefox chromium chrome google-chrome; do
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

for terminal in wezterm kitty st alacritty xterm; do
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
