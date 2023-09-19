set rtp=

set clipboard=unnamed,unnamedplus
set ignorecase
set incsearch
set nomodifiable
set readonly
set smartcase

nnoremap Y y$
nnoremap <expr> q ":qa!\<cr>"

autocmd VimEnter * normal! G{}
