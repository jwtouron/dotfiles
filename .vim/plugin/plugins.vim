vim9script

# bronson/vim-trailing-whitespace
autocmd ColorScheme *
      \ highlight
      \ ExtraWhitespace
      \ guisp=salmon
      \ gui=undercurl
      \ cterm=undercurl
      \ guifg=salmon
      \ guibg=NONE
      \ ctermfg=red
      \ ctermbg=NONE
      \ | match ExtraWhitespace /\\\@<![\u3000[:space:]]\+\%#\@<!$/
