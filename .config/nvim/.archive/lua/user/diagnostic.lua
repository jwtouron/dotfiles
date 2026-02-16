local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.diagnostic.config({
  signs = false,
  -- signs = {
  --   text = {
  --     [vim.diagnostic.severity.ERROR] = "󰅚 ",
  --     [vim.diagnostic.severity.WARN] = "󰀪 ",
  --     [vim.diagnostic.severity.HINT] = "󰌶 ",
  --     [vim.diagnostic.severity.INFO] = " "
  --   },
  -- },
  virtual_text = { prefix = '●', },
})

-- vim.api.nvim_create_autocmd("CursorHold", {
--   group = augroup,
--   callback = function()
--     vim.diagnostic.config({ virtual_lines = { current_line = true, severity = vim.diagnostic.severity.ERROR, } })
--
--     vim.api.nvim_create_autocmd({ "BufEnter", "CursorMoved", "InsertEnter", "TextChanged", "WinEnter", }, {
--       group = augroup,
--       callback = function()
--         vim.diagnostic.config({ virtual_lines = false })
--       end,
--       once = true,
--     })
--   end
-- })

vim.keymap.set("n", "<leader>dd", vim.diagnostic.setloclist, { desc = "Show buffer diagnostics in loclist" })
vim.keymap.set("n", "<leader>dD", vim.diagnostic.setqflist, { desc = "Show all diagnostics in qflist" })
vim.keymap.set("n", "<leader>df", vim.diagnostic.open_float, { desc = "Open diagnostic floating window" })
vim.keymap.set("n", "<leader>ds", vim.diagnostic.open_float, { desc = "Open diagnostic floating window" })

-- local virtual_lines_enabled = false
--
-- local function toggle_virtual_lines()
--   local config = nil
--   if virtual_lines_enabled then
--     config = { virtual_lines = false }
--   else
--     config = { virtual_lines = { current_line = true } }
--   end
--   vim.diagnostic.config(config)
--   virtual_lines_enabled = not virtual_lines_enabled
--   print(virtual_lines_enabled)
-- end
--
-- vim.keymap.set("n", "<C-w>d", toggle_virtual_lines)
-- vim.keymap.set("n", "<C-w><C-d>", toggle_virtual_lines)
