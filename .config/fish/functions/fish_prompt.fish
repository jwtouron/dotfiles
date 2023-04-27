function fish_prompt -d "Write out the prompt"
    if test $status -eq 0
        set -f prompt_color (set_color green)
    else
        set -f prompt_color (set_color red)
    end
    printf '\n%s%s%s\n%s❯%s ' (set_color $fish_color_cwd) (prompt_pwd) (set_color normal) $prompt_color (set_color normal)
end
