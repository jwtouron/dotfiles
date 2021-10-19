setlocal tabstop=4

let g:elm_format_autosave = 0
let g:elm_format_fail_silently = 1

let &l:errorformat = '%E%.%#--\ %m\ -%#\ %f' . ','
let &l:errorformat .= '%C%m:' . ','
let &l:errorformat .= '%C%l|%.%#' . ','
let &l:errorformat .= '%Z%p^^%#' . ','
let &l:errorformat .= '%C%.%#' . ','
let &l:errorformat .= '%-G%.%#'
" setlocal makeprg=elm\ make\ %\ --output=%:t:r.js
