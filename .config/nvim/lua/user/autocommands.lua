local MyAutocommands = vim.api.nvim_create_augroup("MyAutocommands", { clear = true })

vim.api.nvim_create_autocmd("TextYankPost", {
  group = MyAutocommands,
  callback = function() vim.highlight.on_yank() end,
})
