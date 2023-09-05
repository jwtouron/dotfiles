return {
  { "dstein64/vim-startuptime", cmd = "StartupTime", config = function() vim.g.startuptime_tries = 10 end, },
  -- { "junegunn/vim-easy-align", keys = { { "ga", "<Plug>(EasyAlign)", mode = { "n", "x" } } } },
  { "kylechui/nvim-surround", event = "VeryLazy", config = true, },
  { "max397574/better-escape.nvim", event = "VeryLazy", opts = { mapping = { "jk", "kj" }, timeout = 250, } },
  { "mbbill/undotree", cmd = { "UndotreeShow", "UndotreeToggle" } },
  { "nelstrom/vim-visual-star-search", event = "VeryLazy", },
  { "romainl/vim-cool", event = "VeryLazy", config = function() vim.g.cool_total_matches = 1 end },
  { "romainl/vim-qf", init = function() vim.g.qf_mapping_ack_style = 1; vim.g.qf_auto_resize = 0; vim.g.qf_max_height = 0 end, },
  { "tpope/vim-repeat", event = "VeryLazy", },
  { "tpope/vim-rsi", event = "VeryLazy", },
  { "tpope/vim-sleuth", event = "VeryLazy", },
  { "tpope/vim-unimpaired", event = "VeryLazy", },
}
