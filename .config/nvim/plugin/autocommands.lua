local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

-- Jump to last location when opening a buffer.
vim.api.nvim_create_autocmd({ "BufReadPost" }, {
  group = augroup,
  pattern = "*",
  callback = function()
    vim.api.nvim_exec2('silent! normal! g`"zvzz', {})
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = augroup,
  pattern = "*",
  command = "lua vim.hl.on_yank()",
})

vim.api.nvim_create_autocmd('QuickFixCmdPost', {
  group = augroup,
  pattern = { '[^l]*' }, -- quickfix commands
  callback = function() vim.cmd('cwindow') end,
})

vim.api.nvim_create_autocmd('QuickFixCmdPost', {
  group = augroup,
  pattern = { 'l*' }, -- location list commands
  callback = function() vim.cmd('lwindow') end,
})
