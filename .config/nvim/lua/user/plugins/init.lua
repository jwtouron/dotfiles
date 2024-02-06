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
    "romainl/vim-qf",
    event = "QuickFixCmdPre",
    init = function()
      vim.g.qf_mapping_ack_style = 1
      vim.g.qf_auto_resize = 0
      vim.g.qf_max_height = 0
    end,
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
