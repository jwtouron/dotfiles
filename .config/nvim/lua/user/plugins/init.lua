return {
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    config = function()
      require("better_escape").setup({mapping = {"jk", "kj"}})
    end,
  },
  {
    "romainl/vim-cool",
    event = "CmdlineEnter",
    config = function()
      vim.g.cool_total_matches = 1
    end
  },
  {
    "tpope/vim-rsi",
    event = "VeryLazy",
    config = function()
      vim.keymap.set("n", "[<space>", function() vim.cmd [[normal! O]] end)
      vim.keymap.set("n", "]<space>", function() vim.cmd [[normal! o]] end)
    end,
  },
}
