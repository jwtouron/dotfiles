let s:commands = {
      \ 'nimlsp': 'nimble install --nocolor nimlsp',
      \ 'pyls': "python3 -m pip install --no-color 'python-language-server[all]'",
      \ }

function! my_language_server#CompleteCustom(a, l, p) abort
  return join(keys(s:commands), "\n")
endfunction

function! my_language_server#Install(name) abort
  let command = get(s:commands, a:name)
  if [command] == [0]
     echoerr 'Unknown server: ' . a:name
     return
  endif
  exec '!' . command
endfunction
