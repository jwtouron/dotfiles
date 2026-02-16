vim.diagnostic.config {
  severity_sort = true,
  signs = false,
  -- signs = {
  --   text = {
  --     [vim.diagnostic.severity.ERROR] = "󰅚",
  --     [vim.diagnostic.severity.WARN] = "󰀪",
  --     [vim.diagnostic.severity.HINT] = "󰌶",
  --     [vim.diagnostic.severity.INFO] = "",
  --   },
  -- },
  underline = false,
  virtual_text = { prefix = '●', },
}

vim.keymap.set("n", "<leader>dd", vim.diagnostic.setloclist, { desc = "Show buffer diagnostics in loclist" })
vim.keymap.set("n", "<leader>dD", vim.diagnostic.setqflist, { desc = "Show all diagnostics in qflist" })
vim.keymap.set("n", "<leader>df", vim.diagnostic.open_float, { desc = "Open diagnostic floating window" })
vim.keymap.set("n", "<leader>ds", vim.diagnostic.open_float, { desc = "Open diagnostic floating window" })
