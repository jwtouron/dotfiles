local signs = {
  [vim.diagnostic.severity.ERROR] = "󰅚",
  [vim.diagnostic.severity.WARN] = "󰀪",
  [vim.diagnostic.severity.HINT] = "󰌶",
  [vim.diagnostic.severity.INFO] = "",
}

local hl_map = {
  [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
  [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
  [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
  [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
}

vim.diagnostic.config {
  severity_sort = true,
  signs = false,
  -- signs = {
  --   text = signs,
  -- },
  status = {
    format = function(counts)
      local items = {}
      for level in ipairs(vim.diagnostic.severity) do
        local count = counts[level]
        if count then
          table.insert(
            items,
            ("%%#%s#%s %s"):format(hl_map[level], signs[level], count)
          )
        end
      end
      return table.concat(items, " ")
    end
  },
  underline = false,
  virtual_text = { prefix = '●', },
}

local function toggle_virtual_lines()
  local virtual_lines = not (vim.diagnostic.config()['virtual_lines'] or false)
  vim.diagnostic.config { virtual_lines = virtual_lines }
  if virtual_lines then
    vim.api.nvim_create_autocmd({ 'CursorMoved', 'InsertEnter', 'CmdlineEnter', 'BufEnter', 'WinEnter' }, {
      once = true,
      callback = function()
        vim.diagnostic.config { virtual_lines = false }
      end,
    })
  end
end

vim.keymap.set("n", "<leader>dd", vim.diagnostic.setloclist, { desc = "Show buffer diagnostics in loclist" })
vim.keymap.set("n", "<leader>dD", vim.diagnostic.setqflist, { desc = "Show all diagnostics in qflist" })
vim.keymap.set("n", "<leader>df", vim.diagnostic.open_float, { desc = "Open diagnostic floating window" })
vim.keymap.set("n", "<leader>ds", toggle_virtual_lines, { desc = "Toggle diagnostic virtual lines" })
