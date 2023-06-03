-- Documentation:
--
-- It's important that you set up the plugins in the following order:
-- 1. mason.nvim
-- 2. mason-lspconfig.nvim
-- 3. Setup servers via lspconfig

return {
  "williamboman/mason-lspconfig.nvim",
  dependencies = { "neovim/nvim-lspconfig", "williamboman/mason.nvim", },
  -- event: Per documentation, don't make lazy.
  config = function()
    -- require("mason").setup() should already be completed as dependency.
    local mason_lspconfig = require("mason-lspconfig")
    mason_lspconfig.setup {
      ensure_installed = { "lua_ls", },
    }
    mason_lspconfig.setup_handlers(require("user.plugins.lsp.handlers"))
  end,
}
