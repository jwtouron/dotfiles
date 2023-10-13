-- Documentation:
--
-- It's important that you set up the plugins in the following order:
-- 1. mason.nvim
-- 2. mason-lspconfig.nvim
-- 3. Setup servers via lspconfig

local function on_attach(client, bufnr)
  client.server_capabilities.semanticTokensProvider = nil

  if client.server_capabilities.inlayHintProvider then
    vim.lsp.inlay_hint(bufnr, true)
  end

  vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

  local opts = function(desc)
    return { silent = true, buffer = bufnr, desc = desc }
  end

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts "Displays hover information about the symbol under the cursor.")
  vim.keymap.set("n", "gD", vim.lsp.buf.definition, opts "Jumps to the definition of the symbol under the cursor.")
  vim.keymap.set("n", "gd", vim.lsp.buf.declaration, opts "Jumps to the declaration of the symbol under the cursor.")
  vim.keymap.set("n", "gl", vim.diagnostic.open_float, opts "Show diagnostics in a floating window.")

  vim.keymap.set("n", "<localleader>li", vim.lsp.buf.implementation, opts "Lists all the implementations for the symbol under the cursor in the quickfix window.")
  vim.keymap.set("n", "<localleader>lt", vim.lsp.buf.type_definition, opts "Jumps to the definition of the type of the symbol under the cursor.")
  vim.keymap.set("n", "<localleader>lr", vim.lsp.buf.references, opts "Lists all the references to the symbol under the cursor in the quickfix window.")
  vim.keymap.set("n", "<localleader>ls", vim.lsp.buf.signature_help, opts "Displays signature information about the symbol under the cursor in a floating window.")
  vim.keymap.set("n", "<localleader>lcr", vim.lsp.buf.rename, opts "Renames all references to the symbol under the cursor.")
  vim.keymap.set("n", "<localleader>lcf", vim.lsp.buf.format, opts "Format code in current buffer.")
  vim.keymap.set("n", "<localleader>lca", vim.lsp.buf.code_action, opts "Selects a code action available at the current cursor position.")
  vim.keymap.set("n", "<localleader>ld", vim.diagnostic.open_float, opts "Show diagnostics in a floating window.")

  vim.keymap.set({ "n", "v", }, "gm", vim.lsp.buf.format, opts "Format code.")
end

local function init_handlers()
  local lspconfig = require("lspconfig")
  local capabilities = require('cmp_nvim_lsp').default_capabilities
  return {
    function(server_name)
      lspconfig[server_name].setup {
        capabilites = capabilities,
        on_attach = on_attach,
      }
    end,

    efm = function()
      lspconfig.efm.setup {
          capabilites = capabilities,
          on_attach = on_attach,
          init_options = { documentFormatting = true },
        }
    end,

    lua_ls = function()
      lspconfig.lua_ls.setup {
        capabilites = capabilities,
        on_attach = on_attach,
        settings = {
          Lua = {
            runtime = {
              -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
              version = 'LuaJIT',
            },
            diagnostics = {
              -- Get the language server to recognize the `vim` global
              globals = {'vim'},
            },
            workspace = {
              -- Make the server aware of Neovim runtime files
              library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
              enable = false,
            },
          },
        }
      }
    end,
  }
end

return {
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies =  "williamboman/mason.nvim",
    -- event: Per documentation, don't make lazy.
    config = function()
      -- require("mason").setup() should already be completed as dependency.
      local mason_lspconfig = require("mason-lspconfig")
      mason_lspconfig.setup {
        ensure_installed = { "efm", "jsonls", "lua_ls", "marksman", },
      }
      mason_lspconfig.setup_handlers(init_handlers())
    end,
  },

  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    dependencies = { "williamboman/mason.nvim", "williamboman/mason-lspconfig.nvim", },
    opts = {
      inlay_hints = { enabled = true },
    },
    config = function()
      -- Setup Diagnostics
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
  },

  {
    "j-hui/fidget.nvim",
    tag = "legacy",
    event = "LspAttach",
    config = true,
  },

  { "folke/neodev.nvim", config = true },
}
