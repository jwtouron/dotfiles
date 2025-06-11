local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

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
      if client.workspace_folders then
        local path = client.workspace_folders[1].name
        if path ~= vim.fn.stdpath('config') and (vim.uv.fs_stat(path..'/.luarc.json') or vim.uv.fs_stat(path..'/.luarc.jsonc')) then
          return
        end
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
          -- or pull in all of 'runtimepath'. NOTE: this is a lot slower and will cause issues when working on your own configuration (see https://github.com/neovim/nvim-lspconfig/issues/3189)
          -- library = vim.api.nvim_get_runtime_file("", true)
        }
      })
    end,
    settings = {
      Lua = {}
    }
  },
}

return {

  {
    "neovim/nvim-lspconfig",
    dependencies = 'williamboman/mason.nvim',
    event = "FileType",
    init = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = augroup,
        callback = function(ev)
          vim.opt_local.signcolumn = 'yes:1'

          local client = vim.lsp.get_client_by_id(ev.data.client_id)
          if client then client.server_capabilities.semanticTokensProvider = nil end

          vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "LSP Goto definition" })
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "LSP Goto declaration" })
        end,
      })
    end,
    config = function()
      local mason_registry = require("mason-registry")
      local package_names = mason_registry.get_installed_package_names()
      for _, package_name in ipairs(package_names) do
        local lspconfig_name = package_to_lspconfig[package_name] or package_name
        if server_configs[lspconfig_name] then
          vim.lsp.config(lspconfig_name, server_configs[lspconfig_name])
        end
        vim.lsp.enable(lspconfig_name)
      end
    end,
  },

  {
    "mason-org/mason.nvim",
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
