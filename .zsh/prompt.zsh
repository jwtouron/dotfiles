exitcode='%(?.${PR_BOLD_GREEN}√ $?.${PR_BOLD_RED}X $?)%{$reset_color%}'
dir="$PR_BOLD_CYAN%1~%{$reset_color%}"
PROMPT="$exitcode $dir %# "
