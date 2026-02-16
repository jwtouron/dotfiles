local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

local mason_to_lsp_name = setmetatable(
  {
    ["lua-language-server"] = "lua_ls",
  },
  { __index = function(_, s) return s end }
)

vim.pack.add(
  {
    'https://github.com/neovim/nvim-lspconfig',
    'https://github.com/mason-org/mason.nvim',
    'https://github.com/j-hui/fidget.nvim',
  },
  { confirm = false, }
)

require('mason').setup()
local mason_registry = require('mason-registry')
for _, name in ipairs(mason_registry.get_installed_package_names()) do
  vim.lsp.enable(mason_to_lsp_name[name])
end

local setup
setup = function()
  require('fidget').setup()
  setup = function() end
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = augroup,
  callback = function(args)
    setup()

    vim.opt_local.complete = "o"

    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
    client.server_capabilities.semanticTokensProvider = nil

    vim.lsp.document_color.enable(false, args.buf)

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = true, desc = "LSP Goto definition" })
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { buffer = true, desc = "LSP Goto declaration" })
    vim.keymap.set("n", "grh", function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end , { buffer = true, desc = "LSP Toggle Inlay Hints" })
  end,
})
