local function setup_diagnostic()
  vim.diagnostic.config({
    severity_sort = true,
  })
end

local function setup_icons()
  local icons = {
    Class = " ",
    Color = " ",
    Constant = " ",
    Constructor = " ",
    Enum = "了 ",
    EnumMember = " ",
    Field = " ",
    File = " ",
    Folder = " ",
    Function = " ",
    Interface = "ﰮ ",
    Keyword = " ",
    Method = "ƒ ",
    Module = " ",
    Property = " ",
    Snippet = "﬌ ",
    Struct = " ",
    Text = " ",
    Unit = " ",
    Value = " ",
    Variable = " ",
  }

  local kinds = vim.lsp.protocol.CompletionItemKind
  for i, kind in ipairs(kinds) do
    kinds[i] = icons[kind] or kind
  end
end

local function setup_signs()
  local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
  for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
  end
end

return {
  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    config = function()
      setup_diagnostic()
      setup_icons()
      setup_signs()
    end
  }
}
