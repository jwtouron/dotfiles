vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.api.nvim_set_keymap("i", "jk", "<ESC>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "kj", "<ESC>", { noremap = true, silent = true })

vim.cmd [[
nnoremap <expr> n v:searchforward ? 'n' : 'N'
nnoremap <expr> N v:searchforward ? 'N' : 'n'
]]
