local function setup_diagnostics()
  vim.diagnostic.config({
    severity_sort = true,
    virtual_text = { prefix = '●', }
  })

  local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
  for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
  end
end

return {
  "neovim/nvim-lspconfig",
  event = "VeryLazy",
  config = function()
    setup_diagnostics()
  end
}
