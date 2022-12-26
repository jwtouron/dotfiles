function! my_coc#Setup() abort
  return
  setlocal nobackup
  setlocal nowritebackup
  setlocal updatetime=300
  setlocal signcolumn=number

  inoremap <buffer> <silent> <expr> <TAB>
        \ coc#pum#visible() ? coc#pum#next(1):
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()
  inoremap <buffer> <expr> <S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

  inoremap <buffer> <silent> <expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                                \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

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
    if CocAction('hasProvider', 'hover')
      call CocActionAsync('doHover')
    else
      call feedkeys('K', 'in')
    endif
  endfunction

  nmap <buffer> <localleader>rn <Plug>(coc-rename)

  augroup mygroup
    autocmd!
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  augroup end

  xmap <buffer> <localleader>a  <Plug>(coc-codeaction-selected)
  nmap <buffer> <localleader>a  <Plug>(coc-codeaction-selected)

  nmap <buffer> <localleader>ac  <Plug>(coc-codeaction)
  nmap <buffer> <localleader>qf  <Plug>(coc-fix-current)

  nmap <localleader>cl  <Plug>(coc-codelens-action)

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
