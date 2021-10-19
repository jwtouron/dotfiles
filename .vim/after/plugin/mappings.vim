let mapleader = "\<space>"
let maplocalleader = "\\"

nnoremap <expr> gV '`[' . getregtype()[0] . '`]'
" nnoremap <expr> j v:count ? 'j' : 'gj'
" nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap Y y$

nmap cg* *Ncgn
xmap cg* *Ncgn

nnoremap <expr> n v:searchforward ? 'n' : 'N'
nnoremap <expr> N v:searchforward ? 'N' : 'n'

nnoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
nnoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

Arpeggio inoremap jk <Esc>

nmap <F5> <Plug>(qf_qf_toggle)

nnoremap <leader>b :Buffers<cr>

nnoremap <leader>cc :CSearchSearchSmart<cr>
nnoremap <leader>ci :CSearchIndex 

nnoremap <leader>f :FZF<cr>

nmap <leader>t <plug>(my-terminal-sendkeys)
xmap <leader>t <plug>(my-terminal-sendkeys)
nmap <leader>tl 0vg_<plug>(my-terminal-sendkeys)
nmap <leader>tp vap<plug>(my-terminal-sendkeys)
nmap <leader>tb ggVG<plug>(my-terminal-sendkeys)<c-o><c-o>

" anyjump
nnoremap <leader>jj :AnyJump<CR>
xnoremap <leader>jj :AnyJumpVisual<CR>
nnoremap <leader>jb :AnyJumpBack<CR>
nnoremap <leader>jl :AnyJumpLastResults<CR>

nnoremap <leader>g :Grepper<cr>
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

" function s:Grep(...) abort
"   exec 'silent grep! ' .. a:000->join()->shellescape()
"   copen
" endfunction
" command! -nargs=* Grep call <sid>Grep(<f-args>)
" nnoremap <leader>g :Grep 

" function! s:GrepOperator(type) abort
"   let saved_unnamed_register = @@
"   if a:type ==# 'v'
"     normal! `<v`>y
"   elseif a:type ==# 'char'
"     normal! `[v`]y
"   else
"     return
"   endif
"   silent execute "grep! -R " . shellescape(@@) . " ."
"   copen
"   let @@ = saved_unnamed_register
" endfunction

" nnoremap gs :set operatorfunc=<SID>GrepOperator<cr>g@
" vnoremap gs :<c-u>call <SID>GrepOperator(visualmode())<cr>
