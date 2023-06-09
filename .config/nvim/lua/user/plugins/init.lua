return {
  { "AckslD/muren.nvim", cmd = { "MurenToggle", "MurenOpen", "MurenFresh", "MurenUnique" }, config = true },
  { "ixru/nvim-markdown", ft = "markdown" },
  { "kylechui/nvim-surround", event = "VeryLazy", config = true, },
  { "max397574/better-escape.nvim", event = "VeryLazy", opts = { mapping = { "jk", "kj" }, timeout = 250, } },
  { "mbbill/undotree", cmd = { "UndotreeShow", "UndotreeToggle" } },
  { "romainl/vim-cool", event = "VeryLazy", },
  { 'stevearc/oil.nvim', dependencies = { "nvim-tree/nvim-web-devicons" }, config = true, },
  { "tpope/vim-fugitive", cmd = { "G", "Git", "GitGrep" } },
  { "tpope/vim-repeat", event = "VeryLazy", },
  { "tpope/vim-rsi", event = "VeryLazy", },
  { "tpope/vim-sleuth", event = "VeryLazy", },
}
