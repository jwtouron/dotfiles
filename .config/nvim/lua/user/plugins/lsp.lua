local server_configs = {
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

  pyright = {},

  rust_analyzer = {},
}

return {
  {
    "neovim/nvim-lspconfig",
    dependencies = 'hrsh7th/cmp-nvim-lsp',
    event = "VeryLazy",
    init = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = MyAugroup,
        pattern = "*",
        callback = function(ev)
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          local opts = { buffer = ev.buf }

          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
          vim.keymap.set('i', '<C-l>', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)

          vim.opt_local.signcolumn = 'yes'
        end,
      })
    end,
    config = function()
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
      local lspconfig = require("lspconfig")
      for server, config in pairs(server_configs) do
        config = vim.tbl_extend("error", config, { capabilities = capabilities })
        lspconfig[server].setup(config)
      end
    end,
  },
  {
    "williamboman/mason.nvim",
    event = "VeryLazy",
    opts = {},
  },
  {
    "folke/neodev.nvim",
    event = "VeryLazy",
    opts = {}
  },
  {
    "j-hui/fidget.nvim",
    event = "VeryLazy",
    opts = {},
  },
}
