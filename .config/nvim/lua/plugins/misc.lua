return {
  { "AckslD/muren.nvim", event = "VeryLazy", config = true },
  { "akinsho/toggleterm.nvim", event = "VeryLazy" },
  { "ixru/nvim-markdown", ft = "markdown" },
  -- { "kylechui/nvim-surround", version = "*", event = "VeryLazy",config = true },
  { "max397574/better-escape.nvim", opts = { mapping = { "jk", "kj" } } },
  { "mbbill/undotree", cmd = { "UndotreeShow", "UndotreeToggle" } },
  {
    "norcalli/nvim-colorizer.lua",
    event = "VeryLazy",
    config = function()
      require("colorizer").setup()
    end,
  },
  { "romainl/vim-cool", event = "VeryLazy" },
  { "stevearc/oil.nvim", config = true },
  { "tpope/vim-fugitive", cmd = { "G", "Git", "Ggrep" } },
  { "tpope/vim-rsi", event = "VeryLazy" },
}
