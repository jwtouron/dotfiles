-- Reference: https://github.com/williamboman/mason-lspconfig.nvim/blob/main/lua/mason-lspconfig/mappings/server.lua
local package_to_lspconfig = {
  ["docker-compose-language-service"] = "docker_compose_language_service",
  ["dockerfile-language-server"] = "dockerls",
  ["json-lsp"] = "jsonls",
  ["lua-language-server"] = "lua_ls",
  ["rust-analyzer"] = "rust_analyzer",
}

local server_configs = {
  lua_ls = {
    on_init = function(client)
      local path = client.workspace_folders[1].name
      if vim.loop.fs_stat(path..'/.luarc.json') or vim.loop.fs_stat(path..'/.luarc.jsonc') then
        return
      end

      client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
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
            -- Depending on the usage, you might want to add additional paths here.
            -- "${3rd}/luv/library"
            -- "${3rd}/busted/library",
          }
          -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
          -- library = vim.api.nvim_get_runtime_file("", true)
        }
      })
    end,
    settings = {
      Lua = {}
    }
  }

  -- basedpyright = {},
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
}

return {

  {
    "neovim/nvim-lspconfig",
    dependencies = { 'hrsh7th/cmp-nvim-lsp', 'williamboman/mason.nvim' },
    event = "VeryLazy",
    init = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = MyAugroup,
        pattern = "*",
        callback = function(ev)
          local client = vim.lsp.get_client_by_id(ev.data.client_id)
          client.server_capabilities.semanticTokensProvider = nil

          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          local opts = { buffer = ev.buf }

          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          -- vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
          vim.keymap.set('i', '<C-l>', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)

          vim.opt_local.signcolumn = 'yes'
        end,
      })
    end,
    config = function()
      -- Setup servers
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
      local lspconfig = require("lspconfig")
      local mason_registry = require("mason-registry")
      local package_names = mason_registry.get_installed_package_names()
      for _, package_name in pairs(package_names) do
        local lspconfig_name = package_to_lspconfig[package_name] or package_name
        local config = { capabilities = capabilities }
        if server_configs[lspconfig_name] then
          config = vim.tbl_extend("error", server_configs[lspconfig_name], config)
        end
        lspconfig[lspconfig_name].setup(config)
      end

      vim.api.nvim_create_user_command('LspCodeAction', function()
        vim.lsp.buf.code_action()
      end,
      {})

      vim.api.nvim_create_user_command('LspRename', function(arg)
        vim.lsp.buf.rename(arg.fargs[1])
      end,
      { nargs = '?' })
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
