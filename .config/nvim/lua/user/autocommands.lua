vim.api.nvim_create_autocmd("FileType", {
  group = MyAugroup,
  pattern = "lua",
  command = "setlocal tabstop=2",
})

vim.api.nvim_create_autocmd("FileType", {
  group = MyAugroup,
  pattern = "help",
  command = "nnoremap q <cmd>q<cr>",
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = MyAugroup,
  pattern = "*",
  command = "lua vim.highlight.on_yank()",
})

vim.api.nvim_create_autocmd("ColorScheme", {
  group = MyAugroup,
  pattern = "*",
  command = "highlight MatchParen term=underline cterm=underline gui=underline ctermbg=NONE guibg=NONE",
})
