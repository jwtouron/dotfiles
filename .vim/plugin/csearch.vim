if exists('g:loaded_csearch') || &cp
  finish
endif
let g:loaded_csearch = 1

function! CSearchIndex(index_dir, ...) abort
  let old_csearchindex = $CSEARCHINDEX
  try
    let $CSEARCHINDEX = a:index_dir . '/.csearchindex'
    let l:dirs = join(a:000)
    execute '! cindex ' . l:dirs
    if v:shell_error != 0
      echoe 'cindex exited with non-zero: ' . v:shell_error
      return
    endif
  finally
    let $CSEARCHINDEX = old_csearchindex
  endtry
endfunction

function! CSearchSearch(index_dir, regexp) abort
  let old_csearchindex = $CSEARCHINDEX
  try
    let $CSEARCHINDEX = a:index_dir . '/.csearchindex'
    let output = map(split(system('csearch -n ''' . a:regexp . ''''), "\n"), 'trim(v:val)')
    return output
  finally
    let $CSEARCHINDEX = old_csearchindex
  endtry
endfunction

function! CSearchLineToQFItem(line, ...) abort
  let col = 0
  let matches = matchlist(a:line, '\(.:\?[^:]\{-}\)' . ':' . '\(\d\{-}\)' . ':' . '\(\D.\+\)')
  if len(matches) > 0
    let [_, filename, lnum, text, _, _, _, _, _, _] = matches
    if a:0
      let idx = match(text, a:1)
      if idx >= 0
        let col = idx
      endif
    endif
    let type = ''
  else
    let [filename, lnum, text] = ["", 0, a:line]
    let type = 'E'
  endif
  let output = {'filename': filename,
        \ 'lnum': lnum,
        \ 'col': col,
        \ 'text': text,
        \ 'type': type,
        \ }
  return output
endfunction

function! s:FindCSearchIndexDir() abort
  let index_path = findfile('.csearchindex', expand('%:p:h') . ';')
  if index_path ==# ''
    echoe 'Cannot find .csearchindex file!'
    return
  endif
  return fnamemodify(index_path, ':p:h')
endfunction

function! s:CSearchSearchCommand(index_dir, regexp) abort
  let output = CSearchSearch(a:index_dir, a:regexp)
  let qflist = map(output, 'CSearchLineToQFItem(v:val)')
  call setqflist(qflist)
endfunction

command! -nargs=+ -bar -complete=dir CSearchIndex call CSearchIndex(<f-args>)
command! -nargs=+ -bar CSearchSearch call <sid>CSearchSearchCommand(<f-args>)

let s:ALL = 'all'

let s:file_types = {
      \ s:ALL: [],
      \ 'c': ['c', 'h'],
      \ 'clojure': ['clj', 'cljc', 'cljs'],
      \ 'cpp': ['cpp', 'cxx', 'h'],
      \ 'haskell': ['hs'],
      \ 'python': ['py'],
      \ 'vim': ['vim'],
      \ }


function! CSearchFileTypeComplete(arg_lead, cmd_line, cursor_pos) abort
  return keys(s:file_types)
endfunction

function! s:input(prompt, ...) abort
  call inputsave()
  try
    return call('input', [a:prompt] + a:000)
  finally
    call inputrestore()
  endtry
endfunction

function! s:CSearchSearchSmartCommand() abort
  let regexp = <sid>input('Regexp: ', expand('<cword>'))
  let file_type = <sid>input('File type: ', &filetype, "customlist,CSearchFileTypeComplete")
  let directory = <sid>input('Directory: ', <sid>FindCSearchIndexDir(), 'dir')
  echo "\n"

  if !has_key(s:file_types, file_type)
    echoe 'Unknown file type: ' . file_type
    return
  endif

  let output = CSearchSearch(directory, regexp)
  if file_type ==# s:ALL|| file_type ==# ''
    let qflist = map(output, 'CSearchLineToQFItem(v:val)')
  else
    let qflist = []
    for line in output
      let item = CSearchLineToQFItem(line)
      let ext = fnamemodify(item['filename'], ':e')
      if item['type'] == 'E' || index(s:file_types[file_type], ext) != -1
        let qflist = add(qflist, item)
      endif
    endfor
  endif
  call setqflist(qflist)
  copen
endfunction

command! -bar CSearchSearchSmart call <sid>CSearchSearchSmartCommand()
