function git_prompt_info {
  local ref=$(=git symbolic-ref HEAD 2> /dev/null)
  local gitst="$(=git status 2> /dev/null)"
  local pairname=${${${GIT_AUTHOR_EMAIL#pair+}%@github.com}//+/\/}
  if [[ ${pairname} == 'ch' || ${pairname} == '' ]]; then
    pairname=''
  else
    pairname=" ($pairname)"
  fi

  if [[ -f .git/MERGE_HEAD ]]; then
    if [[ ${gitst} =~ "unmerged" ]]; then
      gitstatus=" $PR_BOLD_REDunmerged%{$reset_color%}"
    else
      gitstatus=" $PR_BOLD_GREENmerged%{$reset_color%}"
    fi
  elif [[ ${gitst} =~ "Changes to be committed" ]]; then
    gitstatus=" $PR_BOLD_BLUE!%{$reset_color%}"
  elif [[ ${gitst} =~ "use \"git add" ]]; then
    gitstatus=" $PR_BOLD_RED!%{$reset_color%}"
  elif [[ -n `git checkout HEAD 2> /dev/null | grep ahead` ]]; then
    gitstatus=" $PR_BOLD_YELLOW*%{$reset_color%}"
  else
    gitstatus=''
  fi

  if [[ -n $ref ]]; then
    echo "$PR_BOLD_GREEN/${ref#refs/heads/}%{$reset_color%}$gitstatus$pairname"
  fi
}

loc="$PR_BOLD_GREEN%n@%m%{$reset_color%}"
dir="$PR_BOLD_CYAN%1~%{$reset_color%}"
git="$(git_prompt_info)"
PROMPT='$loc $dir $git$PR_BOLD_WHITE%#%{$reset_color%} '

check="$(echo -e "\xe2\x9c\x93")"
cross="$(echo -e "\xe2\x9c\x97")"
RPROMPT='%(?.$PR_BOLD_GREEN$check $?.$PR_BOLD_RED$cross $?)%{$reset_color%}'
