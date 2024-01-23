vim9script

nnoremap cg* *Ncgn
nnoremap J mzJ`z
nnoremap Q @q
nnoremap Y y$

xnoremap p 0p
xnoremap P 0P

inoremap jk <esc>
inoremap kj <esc>

nnoremap <expr> j v:count == 0 ? 'gj' : 'j'
nnoremap <expr> k v:count == 0 ? 'gk' : 'k'

nnoremap <expr> <silent> n v:searchforward ? 'n' : 'N'
xnoremap <expr> <silent> n v:searchforward ? 'n' : 'N'
onoremap <expr> <silent> n v:searchforward ? 'n' : 'N'
nnoremap <expr> <silent> N v:searchforward ? 'N' : 'n'
xnoremap <expr> <silent> N v:searchforward ? 'N' : 'n'
onoremap <expr> <silent> N v:searchforward ? 'N' : 'n'

nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>

nnoremap <leader>t :NERDTreeToggle<cr>

# FZF
nnoremap <leader><leader> :Files<cr>
nnoremap <leader>, :Buffers<cr>
nnoremap <leader>/ :BLines<cr>
nnoremap <leader>] :BTags<cr>

nnoremap <leader>zb :Buffers<cr>
nnoremap <leader>zf :Files<cr>
nnoremap <leader>zh :Helptags<cr>
nnoremap <leader>zl :Lines<cr>
nnoremap <leader>zm :Marks<cr>
nnoremap <leader>zo :History<cr>
nnoremap <leader>zr :RG<cr>
