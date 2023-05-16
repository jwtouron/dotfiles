local lspconfig = require("lspconfig")

local function on_attach(_, bufnr)
  vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"

  local x
  local map = function(keys, func, desc)
    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = "LSP: "..desc})
  end

  map('gd', vim.lsp.buf.definition, "Goto Definition")
  map('gD', vim.lsp.buf.declaration, "Goto Declaration")
  map('gi', vim.lsp.buf.implementation, "Goto Implementation")
  map('gl', vim.diagnostic.open_float, "Open Diagnostic Float")
  map('go', vim.lsp.buf.type_definition, "Goto Type Definition")
  map('gr', "<cmd>Trouble lsp_references<cr>", "Find References")
  map('gs', vim.lsp.buf.signature_help, "Signature Help")
  map('K', vim.lsp.buf.hover, "Hover")
  map('<leader>wa', vim.lsp.buf.add_workspace_folder, "Add Workspace Folder")
  map('<leader>wr', vim.lsp.buf.remove_workspace_folder, "Remove Workspace Folder")
  map('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end)
  map('<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', "Rename")
  map('<f3>', function()
    vim.lsp.buf.format { async = true }
  end, "Format")
  vim.keymap.set({ 'n', 'v' }, '<f4>', vim.lsp.buf.code_action, "Code Action")

  vim.api.nvim_create_autocmd("CursorHold", {
    buffer = bufnr,
    callback = function()
      local float_opts = {
        focusable = false,
        close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
        border = 'rounded',
        source = 'always',
        prefix = ' ',
        scope = 'cursor',
      }
      vim.diagnostic.open_float(nil, float_opts)
    end
  })
end

local server_settings = {
  lua_ls = {
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
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

return {
  function (server_name)
    local opts = { on_attach = on_attach, }
    local settings = server_settings[server_name]
    if settings then
      opts.settings = settings
    end
    lspconfig[server_name].setup(opts)
  end,
}
