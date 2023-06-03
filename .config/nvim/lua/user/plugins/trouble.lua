local keys = {
  { "<leader>xx", "<cmd>TroubleToggle<cr>",                       { silent = true, desc = "Toggle" } },
  { "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", { silent = true, desc = "[W]orkspace Diagnostics" } },
  { "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>",  { silent = true, desc = "[D]ocument Diagnostics" } },
  { "<leader>xl", "<cmd>TroubleToggle loclist<cr>",               { silent = true, desc = "[L]oclist" } },
  { "<leader>xq", "<cmd>TroubleToggle quickfix<cr>",              { silent = true, desc = "[Q]uickfix" } },
  { "gR",         "<cmd>TroubleToggle lsp_references<cr>",        { silent = true, desc = "LSP References" } },
}

return {
  "folke/trouble.nvim",
  dependencies = { "nvim-telescope/telescope.nvim", "nvim-tree/nvim-web-devicons" },
  keys = function()
    local ret = {}
    for i, key in ipairs(keys) do
      ret[i] = { key[1], nil, desc = key[3].desc }
    end
    return ret
  end,
  config = function()
    for _, key in ipairs(keys) do
      vim.keymap.set("n", key[1], key[2], key[3])
    end

    local trouble = require("trouble.providers.telescope")
    local telescope = require("telescope")
    telescope.setup {
      defaults = {
        mappings = {
          i = { ["<c-t>"] = trouble.open_with_trouble },
          n = { ["<c-t>"] = trouble.open_with_trouble },
        },
      },
    }
  end,
}
