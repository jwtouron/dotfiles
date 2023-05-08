function __command
    command -v $argv[1] >/dev/null
end

# Set the first argument (an environment variable)
# to the first program found in the remaining arguments.
# Example: __set_variable EDITOR nvim vim vi nano
function __set_variable
    for p in $argv[2..]
        if __command $p
            set -g $argv[1] $p
            break
        end
    end
end

if status is-login
#if status is-interactive
    set -p PATH $HOME/.local/bin $HOME/bin $HOME/.ghcup/bin $HOME/.cabal/bin $HOME/.nimble/bin

    __set_variable BROWSER firefox brave brave-browser chromium chrome google-chrome
    __set_variable EDITOR nvim vim vi nano
    __set_variable TERMINAL st wezterm alacritty xterm

    if __command rg
        set -g FZF_DEFAULT_COMMAND "rg --files --hidden --glob '!.git/*'"
    end

    test -f /opt/homebrew/bin/brew && eval (/opt/homebrew/bin/brew shellenv)
end

if status is-interactive
    # Abbreviations

    abbr --add dotdot --regex '^\.\.+$' --function multicd
    abbr --add lc wc -l
    abbr --add ll ls -alhF
    abbr --add ltr ls -alhFtr

    # Aliases

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'

    alias config='/usr/bin/git --git-dir=$HOME/.config/.dotfiles/ --work-tree=$HOME'
    alias config-sync='GIT_DIR=$HOME/.config/.dotfiles GIT_WORK_TREE=$HOME git-sync'
    command -v fdfind > /dev/null && alias fd='fdfind'

    # Environment variables

    set -g fish_greeting
end
