return {
  { "AckslD/muren.nvim", cmd = { "MurenToggle", "MurenOpen", "MurenFresh", "MurenUnique" }, config = true },
  { "dstein64/vim-startuptime", cmd = "StartupTime", config = function() vim.g.startuptime_tries = 10 end, },
  { "gabrielpoca/replacer.nvim", cmd = "Replacer", init = function() vim.api.nvim_create_user_command("Replacer", function() require("replacer").run() end, {}) end, },
  -- { "junegunn/vim-easy-align", keys = { { "ga", "<Plug>(EasyAlign)", mode = { "n", "x" } } } },
  { "kylechui/nvim-surround", event = "VeryLazy", config = true, },
  { "max397574/better-escape.nvim", event = "VeryLazy", opts = { mapping = { "jk", "kj" }, timeout = 250, } },
  { "mbbill/undotree", cmd = { "UndotreeShow", "UndotreeToggle" } },
  { "nelstrom/vim-visual-star-search", event = "VeryLazy", },
  { "romainl/vim-cool", event = "VeryLazy", config = function() vim.g.cool_total_matches = 1 end },
  { "tpope/vim-repeat", event = "VeryLazy", },
  { "tpope/vim-rsi", event = "VeryLazy", },
  { "tpope/vim-sleuth", event = "VeryLazy", },
}
