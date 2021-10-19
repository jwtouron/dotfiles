PATH="$HOME/.local/bin:$HOME/bin:$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.nimble/bin:$PATH"

for terminal in st xterm kitty termite; do
    if command -v "$terminal" >/dev/null; then
        TERMINAL="$terminal"
        break
    fi
done
export TERMINAL
