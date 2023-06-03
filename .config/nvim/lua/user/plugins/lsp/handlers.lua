-- For server configurations, see:
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

local lspconfig = require("lspconfig")

local function on_attach(_, bufnr)
  vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

  local opts = function(desc)
    return { silent = true, buffer = bufnr, desc = desc }
  end

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts "Displays hover information about the symbol under the cursor.")
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts "Jumps to the definition of the symbol under the cursor.")
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts "Jumps to the declaration of the symbol under the cursor.")
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts "Lists all the implementations for the symbol under the cursor in the quickfix window.")
  vim.keymap.set("n", "go", vim.lsp.buf.type_definition, opts "Jumps to the definition of the type of the symbol under the cursor.")
  vim.keymap.set("n", "gr", vim.lsp.buf.references, opts "Lists all the references to the symbol under the cursor in the quickfix window.")
  vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, opts "Displays signature information about the symbol under the cursor in a floating window.")
  vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, opts "Renames all references to the symbol under the cursor.")
  vim.keymap.set("n", "<F3>", vim.lsp.buf.format, opts "Format code in current buffer.")
  vim.keymap.set("n", "<F4>", vim.lsp.buf.code_action, opts "Selects a code action available at the current cursor position.")
  vim.keymap.set("n", "gl", vim.diagnostic.open_float, opts "Show diagnostics in a floating window.")

  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts "Selects a code action available at the current cursor position.")
  vim.keymap.set("n", "<leader>cf", vim.lsp.buf.format, opts "Format code in current buffer.")
  vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, opts "Renames all references to the symbol under the cursor.")

  vim.keymap.set({ "n", "v", }, "gm", vim.lsp.buf.format, opts "Format code.")
end

local capabilities = require('cmp_nvim_lsp').default_capabilities

return {
  function(server_name)
    lspconfig[server_name].setup {
      capabilites = capabilities,
      on_attach = on_attach,
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
