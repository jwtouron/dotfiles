vim.cmd [[
try
    lua vim.g.transparent_background = true
    colorscheme darkplus
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme default
    set background=dark
endtry
]]
