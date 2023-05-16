local user_group = vim.api.nvim_create_augroup("User", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group = user_group,
  pattern = "lua",
  callback = function()
    vim.opt_local.tabstop = 2
  end
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = user_group,
  callback = function()
    vim.highlight.on_yank()
  end,
})
