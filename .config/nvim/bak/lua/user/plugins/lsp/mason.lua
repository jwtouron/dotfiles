-- local border = {
--   {"🭽", "FloatBorder"},
--   {"▔", "FloatBorder"},
--   {"🭾", "FloatBorder"},
--   {"▕", "FloatBorder"},
--   {"🭿", "FloatBorder"},
--   {"▁", "FloatBorder"},
--   {"🭼", "FloatBorder"},
--   {"▏", "FloatBorder"},
-- }

-- LSP settings (for overriding per client)
-- local handlers =  {
--   ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, {border = border}),
--   ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, {border = border }),
-- }
-- handlers = {}

return {
  {
    "williamboman/mason.nvim",
    build = ":MasonUpdate",
    event = "VeryLazy",
    config = true,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "williamboman/mason.nvim",
    event = "VeryLazy",
    config = function()
      local mason_lspconfig = require("mason-lspconfig")
      local handlers = require("user.plugins.lsp.handlers")
      mason_lspconfig.setup { ensure_installed = { "lua_ls", } }
      mason_lspconfig.setup_handlers(handlers)
    end
  },
}
