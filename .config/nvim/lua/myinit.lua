vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.api.nvim_set_keymap("i", "jk", "<ESC>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "kj", "<ESC>", { noremap = true, silent = true })

vim.cmd [[
nnoremap <expr> n v:searchforward ? 'n' : 'N'
nnoremap <expr> N v:searchforward ? 'N' : 'n'
]]

vim.o.autowrite = true
vim.o.ignorecase = true
vim.o.lazyredraw = true
vim.o.list = true
vim.opt.listchars = { tab = "> " }
vim.o.pumheight = 10
vim.o.relativenumber = true
vim.o.shiftround = true
vim.o.showmode = false
vim.o.smartcase = true
vim.o.swapfile = false
vim.opt.virtualedit = { "block" }
vim.o.wrap = false

vim.cmd [[
highlight MatchParen term=underline cterm=underline gui=underline ctermbg=NONE guibg=NONE
]]
