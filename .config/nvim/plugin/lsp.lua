local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.pack.add(
  {
    'https://github.com/neovim/nvim-lspconfig',
    'https://github.com/j-hui/fidget.nvim',
  },
  { confirm = false, }
)

vim.lsp.enable("lua_ls")

local setup
setup = function()
  vim.lsp.document_color.enable(false)
  vim.lsp.semantic_tokens.enable(false)

  require('fidget').setup()
  setup = function() end
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = augroup,
  callback = function(args)
    setup()

    vim.opt_local.complete = "o"

    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = true, desc = "LSP Goto definition" })
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { buffer = true, desc = "LSP Goto declaration" })
    vim.keymap.set("n", "grh", function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end , { buffer = true, desc = "LSP Toggle Inlay Hints" })
  end,
})
