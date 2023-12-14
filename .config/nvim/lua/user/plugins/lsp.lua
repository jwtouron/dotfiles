-- NOTE:
-- In case of lag in projects with many files:
-- workspace = {
--   didChangeWatchedFiles = {
--     dynamicRegistration = false
--  }
-- }
-- See: https://github.com/neovim/neovim/issues/23291

-- Server Configurations
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
local servers = {
  clangd = { capabilities = { offsetEncoding = "utf-8" } },  -- TODO: Might be able to change with NVIM 0.10
  -- clangd = {},

  dockerls = {},

  -- efm = {},

  gopls = {},

  lua_ls = {
    on_init = function(client)
      local path = client.workspace_folders[1].name
      if not vim.loop.fs_stat(path..'/.luarc.json') and not vim.loop.fs_stat(path..'/.luarc.jsonc') then
        client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
          Lua = {
            runtime = {
              -- Tell the language server which version of Lua you're using
              -- (most likely LuaJIT in the case of Neovim)
              version = 'LuaJIT'
            },
            -- Make the server aware of Neovim runtime files
            workspace = {
              checkThirdParty = false,
              library = {
                vim.env.VIMRUNTIME
                -- "${3rd}/luv/library"
                -- "${3rd}/busted/library",
              }
              -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
              -- library = vim.api.nvim_get_runtime_file("", true)
            }
          }
        })

        client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
      end
      return true
    end
  },

  rust_analyzer = {},
}

local setup_diagnostics = function()
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Goto next diagnostic.", silent = true })
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Goto previous diagnostic.", silent = true })
  vim.keymap.set("n", "gl", vim.diagnostic.open_float, { desc = "Show diagnostics in a floating window.", silent = true })

  vim.diagnostic.config({
    severity_sort = true,
    virtual_text = { prefix = '●', }
  })

  local signs = { Error = "󰅚 ", Warn = " ", Hint = "󰌶 ", Info = " " }
  for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
  end
end

local on_lsp_attach = function(ev)
  -- Enable completion triggered by <c-x><c-o>
  vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

  -- Buffer local mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local opts = function(desc)
    if desc then desc = "LSP " .. desc end
    return { buffer = ev.buf, desc = desc }
  end
  vim.keymap.set('n',           'gD', vim.lsp.buf.declaration, opts "Goto Declaration")
  vim.keymap.set('n',           'gd', vim.lsp.buf.definition, opts "Goto Definition")
  vim.keymap.set('n',           'gi', vim.lsp.buf.implementation, opts "Goto Implementation")
  vim.keymap.set({ 'n', 'v', }, 'gm', vim.lsp.buf.format, opts "Format")
  vim.keymap.set('n',           'go', vim.lsp.buf.type_definition, opts "Type Definition")
  vim.keymap.set('n',           'gr', vim.lsp.buf.references, opts "References")
  vim.keymap.set('n',           'gs', vim.lsp.buf.signature_help, opts "Signature Help")
  vim.keymap.set('n',           'K', vim.lsp.buf.hover, opts "Hover")

  vim.keymap.set({ 'n', 'v' }, '<localleader>la', vim.lsp.buf.code_action, opts "Code Action")
  vim.keymap.set('n',          '<localleader>lf', function() vim.lsp.buf.format { async = true } end, opts "Format")
  vim.keymap.set('n',          '<localleader>lr', vim.lsp.buf.rename, opts "Rename")
  vim.keymap.set('n',          '<localleader>lwa', vim.lsp.buf.add_workspace_folder, opts "Add Workspace Folder")
  vim.keymap.set('n',          '<localleader>lwl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts "List Workspace Folders")
  vim.keymap.set('n',          '<localleader>lwr', vim.lsp.buf.remove_workspace_folder, opts "Remove Workspace Folder")

  local client = vim.lsp.get_client_by_id(ev.data.client_id)

  client.server_capabilities.semanticTokensProvider = nil

  if client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint then
    vim.lsp.inlay_hint(ev.buf, true)
  end
end

return {
  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    config = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = on_lsp_attach,
      })

      setup_diagnostics()

      local lspconfig = require('lspconfig')
      for server, config in pairs(servers) do
        lspconfig[server].setup(config)
      end
    end,
  },

  { "williamboman/mason.nvim", event = "VeryLazy", config = true, },

  {
    "j-hui/fidget.nvim",
    config = true,
  },

  { "folke/neodev.nvim", config = true },
}
