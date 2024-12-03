local augroup = vim.api.nvim_create_augroup("user.plugins.lsp", { clear = true })

-- Reference: https://github.com/williamboman/mason-lspconfig.nvim/blob/main/lua/mason-lspconfig/mappings/server.lua
local package_to_lspconfig = {
  ["docker-compose-language-service"] = "docker_compose_language_service",
  ["dockerfile-language-server"] = "dockerls",
  ["json-lsp"] = "jsonls",
  ["lua-language-server"] = "lua_ls",
  ["rust-analyzer"] = "rust_analyzer",
  ["python-lsp-server"] = "pylsp",
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
    dependencies = 'williamboman/mason.nvim',
    -- dependencies = { 'hrsh7th/cmp-nvim-lsp', 'williamboman/mason.nvim' },
    event = "FileType",
    -- event = "VeryLazy",
    init = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = augroup,
        pattern = "*",
        callback = function(ev)
          vim.opt_local.signcolumn = 'yes'

          local client = vim.lsp.get_client_by_id(ev.data.client_id)
          if client then client.server_capabilities.semanticTokensProvider = nil end

          -- vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          local mappings = {
            {'n', 'gD', 'declaration'},
            {'n', 'gd', 'definition'},  -- Also, C-]
            {'n', 'gri', 'implementation'},

            -- Default mappings in Neovim nightly
            -- TODO: Remove after new Neovim release
            {'n', 'grn', 'rename'},
            {{ 'n', 'x' }, 'gra', 'code_action'},
            {'n', 'grr', 'references'},
            {'i', '<C-S>', 'signature_help'},
          }

          for _, mapping in ipairs(mappings) do
            vim.keymap.set(
              mapping[1],
              mapping[2],
              vim.lsp.buf[mapping[3]],
              { desc = 'vim.lsp.buf.' .. mapping[3] .. '()', buffer = ev.buf })
          end
        end,
      })
    end,
    config = function()
      local handlers =  {
        ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, {border = 'rounded'}),
        ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, {border = 'rounded'}),
      }

      local capabilities = {}
      -- local capabilities = require('cmp_nvim_lsp').default_capabilities()

      -- Setup servers
      local lspconfig = require("lspconfig")
      local mason_registry = require("mason-registry")
      local package_names = mason_registry.get_installed_package_names()
      for _, package_name in pairs(package_names) do
        local lspconfig_name = package_to_lspconfig[package_name] or package_name
        local config = { capabilities = capabilities, handlers = handlers, }
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
    event = "FileType",
    cmd = "Mason",
    -- event = "VeryLazy",
    opts = {},
  },

  {
    "folke/neodev.nvim",
    event = "FileType",
    -- event = "VeryLazy",
    opts = {}
  },

  {
    "j-hui/fidget.nvim",
    event = "FileType",
    -- event = "VeryLazy",
    opts = {},
  },

}
