GIT_PROMPT_PREFIX="${PR_BOLD_GREEN}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="${PR_BOLD_GREEN}]%{$reset_color%}"
GIT_PROMPT_AHEAD="${PR_BOLD_RED}ANUM%{$reset_color%}"
GIT_PROMPT_BEHIND="${PR_BOLD_CYAN}BNUM%{$reset_color%}"
GIT_PROMPT_MERGING="${PR_BOLD_MAGENTA}⚡%{$reset_color%}"
GIT_PROMPT_UNTRACKED="${PR_BOLD_RED}●%{$reset_color%}"
GIT_PROMPT_MODIFIED="${PR_BOLD_YELLOW}●%{$reset_color%}"
GIT_PROMPT_STAGED="${PR_BOLD_GREEN}●%{$reset_color%}"

parse_git_branch() {
  (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

parse_git_state() {
  local GIT_STATE=""

  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE="$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD} "
  fi

  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    GIT_STATE="$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND} "
  fi

  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
  fi

  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
  fi

  if ! git diff --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
  fi

  if ! git diff --cached --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
  fi

  if [[ -n $GIT_STATE ]]; then
    echo "$GIT_STATE"
  fi

}

git_prompt_string() {
  local git_where="$(parse_git_branch)"
  [ -n "$git_where" ] && echo "$GIT_PROMPT_PREFIX$PR_YELLOW${git_where#(refs/heads/|tags/)} $(parse_git_state)$GIT_PROMPT_SUFFIX "
}

loc="$PR_BOLD_GREEN%n@%m%{$reset_color%}"
dir="$PR_BOLD_CYAN%1~%{$reset_color%}"
git="$(git_prompt_info)"
PROMPT='$loc $dir $(git_prompt_string)$PR_BOLD_WHITE%#%{$reset_color%} '

check="$(echo -e "\xe2\x9c\x93")"
cross="$(echo -e "\xe2\x9c\x97")"
RPROMPT='%(?.$PR_BOLD_GREEN$check $?.$PR_BOLD_RED$cross $?)%{$reset_color%}'
