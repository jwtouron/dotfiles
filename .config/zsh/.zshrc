_clone() {
    local destdir="$ZDOTDIR/${2}"
    [ ! -d "$destdir" ] && command -v git >/dev/null && git clone "https://github.com/${1}/${2}" "$destdir"
}

: "${XDG_STATE_HOME:=$HOME/.local/state}"

# Functionality common to bash and zsh
#

[ -r "$HOME/.config/shell/interactive.sh" ] && . "$HOME/.config/shell/interactive.sh"

# options (man zshoptions)
#

setopt auto_cd
setopt auto_continue
setopt cdable_vars
setopt complete_aliases
setopt extended_glob
setopt hist_ignore_all_dups
setopt hist_verify
setopt interactive_comments
setopt nomatch
setopt notify
setopt share_history

unsetopt beep

# bindings
#

bindkey -e

zstyle ':zle:edit-command-line' editor "$EDITOR"
autoload -Uz edit-command-line
zle -N edit-command-line

bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
bindkey '^]' edit-command-line
bindkey -M emacs '^[;' edit-command-line      # Alt-;   (^[ is ESC)

bindkey '^xp' push-line
bindkey '^x^p' push-line

# Variables (man zshparam)
#

HISTSIZE=1000
SAVEHIST=1000
HISTFILE="$XDG_STATE_HOME/zsh/history"
[[ ! -d "${HISTFILE:h}" ]] && mkdir -p -- "${HISTFILE:h}"

# Completion
#

if command -v fzf >/dev/null; then
    source <(fzf --zsh)

    _clone Aloxaf fzf-tab

    if [ -d "$ZDOTDIR/fzf-tab" ]; then
        # disable sort when completing `git checkout`
        zstyle ':completion:*:git-checkout:*' sort false
        # set descriptions format to enable group support
        # NOTE: don't use escape sequences (like '%F{red}%d%f') here, fzf-tab will ignore them
        zstyle ':completion:*:descriptions' format '[%d]'
        # set list-colors to enable filename colorizing
        zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
        # force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
        zstyle ':completion:*' menu no
        # preview directory's content with ls when completing cd
        zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -1 --color=always $realpath'
        # custom fzf flags
        # NOTE: fzf-tab does not follow FZF_DEFAULT_OPTS by default
        zstyle ':fzf-tab:*' fzf-flags --color=fg:1,fg+:2 --bind=tab:accept
        # To make fzf-tab follow FZF_DEFAULT_OPTS.
        # NOTE: This may lead to unexpected behavior since some flags break this plugin. See Aloxaf/fzf-tab#455.
        zstyle ':fzf-tab:*' use-fzf-default-opts yes
        # switch group using `<` and `>`
        zstyle ':fzf-tab:*' switch-group '<' '>'

        source "$ZDOTDIR/fzf-tab/fzf-tab.plugin.zsh"
    fi
fi

_clone zsh-users zsh-completions
[ -d "$ZDOTDIR/zsh-completions" ] && fpath=($ZDOTDIR/zsh-completions/src $fpath)

autoload -Uz compinit && compinit

# Prompt
#

command -v starship >/dev/null && eval "$(starship init zsh)"

# Private mode
#

alias priv='ZSH_PRIV=1 zsh -il'
[ -n "$ZSH_PRIV" ] && unset HISTFILE

# Zoxide
#

if command -v zoxide >/dev/null; then
    eval "$(zoxide init --cmd cd zsh)"
    unsetopt auto_cd
fi

# Syntax highlighting
#

_clone zsh-users zsh-syntax-highlighting
[ -d "$ZDOTDIR/zsh-syntax-highlighting" ] && source "$ZDOTDIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# autoload -Uz colors && colors
# autoload -Uz select-word-style && select-word-style bash
