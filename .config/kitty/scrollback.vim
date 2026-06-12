set buftype=nowrite
set clipboard=unnamed,unnamedplus
set ignorecase
set incsearch
set keywordprg=man\ -s
set readonly
set smartcase

nnoremap Y y$
nnoremap <expr> q ":qa!\<cr>"

autocmd VimEnter * file kitty\ scrollback\ buffer
autocmd VimEnter * normal! G{}0
