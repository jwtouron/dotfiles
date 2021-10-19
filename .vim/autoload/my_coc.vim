function! my_coc#Setup() abort
  setlocal encoding=utf-8
  setlocal hidden
  setlocal nobackup
  setlocal nowritebackup
  setlocal cmdheight=2
  setlocal updatetime=300
  setlocal shortmess+=c
  setlocal signcolumn=number

  inoremap <buffer> <silent> <expr> <TAB>
        \ pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()
  inoremap <buffer> <expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
  endfunction

  nmap <buffer> <silent> [g <Plug>(coc-diagnostic-prev)
  nmap <buffer> <silent> ]g <Plug>(coc-diagnostic-next)

  nmap <buffer> <silent> <localleader>d <Plug>(coc-definition)
  nmap <buffer> <silent> <localleader>y <Plug>(coc-type-definition)
  nmap <buffer> <silent> <localleader>i <Plug>(coc-implementation)
  nmap <buffer> <silent> <localleader>r <Plug>(coc-references)

  nnoremap <buffer> <silent> K :call <SID>coc_show_documentation()<CR>

  function! s:coc_show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
      call CocActionAsync('doHover')
    else
      execute '!' . &keywordprg . " " . expand('<cword>')
    endif
  endfunction

  nmap <buffer> <localleader>n <Plug>(coc-rename)
  xmap <buffer> <localleader>a <Plug>(coc-codeaction-selected)
  nmap <buffer> <localleader>a <Plug>(coc-codeaction-selected)

  xmap <buffer> if <Plug>(coc-funcobj-i)
  omap <buffer> if <Plug>(coc-funcobj-i)
  xmap <buffer> af <Plug>(coc-funcobj-a)
  omap <buffer> af <Plug>(coc-funcobj-a)
  xmap <buffer> ic <Plug>(coc-classobj-i)
  omap <buffer> ic <Plug>(coc-classobj-i)
  xmap <buffer> ac <Plug>(coc-classobj-a)
  omap <buffer> ac <Plug>(coc-classobj-a)
endfunction

function my_coc#Start() abort
  CocStart
  call my_coc#Setup()
endfunction
