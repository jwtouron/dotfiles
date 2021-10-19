setlocal tabstop=2

let b:ale_fixers = ['brittany']
let b:ale_linters = ['stack-ghc']
let g:ale_haskell_stack_ghc_options  = '-fno-code -v0 -Wall'
" let g:gutentags_ctags_executable_haskell = 'hasktags --ctags -x'

nnoremap <buffer> <expr> <localleader>h ":Hoogle " . expand("<cword>")
nnoremap <buffer> <localleader>c :HoogleClose<cr>
"nnoremap <buffer> <expr> <localleader>h ":Hoogle " . expand("<cword>")
nnoremap <buffer>        <localleader>r :term stack repl<cr>
nnoremap <buffer>        <localleader>l :T :load "%"<cr>
nnoremap <buffer>        <localleader>t :!hasktags --ctags -f .vimtags -a -x .<cr>

if exists('s:loaded_functions')
  finish
endif
let s:loaded_functions = 1

function s:hoogle_web()
  let l:term = input("Hoogle: ", expand("<cword>"))
  execute "OpenBrowser https://hoogle.haskell.org/?hoogle=" . l:term
endfunction

nnoremap <buffer> <localleader>H :call <sid>hoogle_web()<cr>
"command! -nargs=1 Hoogle call s:hoogle('<args>')
