-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- vim.api.nvim_create_autocmd("FileType", {
--   group = vim.api.nvim_create_augroup("UserFileType", { clear = true }),
--   pattern = "lua",
--   callback = function(_)
--     vim.opt_local.tabstop = 2
--   end
-- })

vim.api.nvim_create_user_command(
  "ReadDate",
  "read !date '+\\%Y-\\%m-\\%d'",
  { desc = "Insert the current date as YYYY-MM-DD below the current line." }
)
