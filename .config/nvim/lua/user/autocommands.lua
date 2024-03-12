vim.api.nvim_create_autocmd({ "BufReadPost" }, {
  pattern = "*",
  callback = function()
    vim.api.nvim_exec('silent! normal! g`"zvzz', false)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = MyAugroup,
  pattern = "go",
  command = "setlocal tabstop=8 noexpandtab",
})

vim.api.nvim_create_autocmd("FileType", {
  group = MyAugroup,
  pattern = "help",
  command = "nnoremap q <cmd>q<cr>",
})

vim.api.nvim_create_autocmd("FileType", {
  group = MyAugroup,
  pattern = "lua",
  command = "setlocal tabstop=2",
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = MyAugroup,
  pattern = "*",
  command = "lua vim.highlight.on_yank()",
})
