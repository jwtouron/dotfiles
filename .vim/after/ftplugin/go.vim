vim9script

setlocal tabstop=8 noexpandtab

def GoDoc()
  var query = input("go doc: ", expand('<cword>'))
  if !empty(trim(query))
    split
    enew
    execute "read !go doc" query
    redraw
    normal! gg
    setl nomodifiable
    nnoremap <buffer> q :q<cr>
  endif
enddef

nnoremap <buffer> <localleader>d :call <SID>GoDoc()<CR>
