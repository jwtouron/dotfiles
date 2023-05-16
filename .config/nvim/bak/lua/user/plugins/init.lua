local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  { "AckslD/muren.nvim", config = true, },
  { "ixru/nvim-markdown", ft = "markdown" },
  { "kylechui/nvim-surround", event = "VeryLazy", config = true, },
  { "max397574/better-escape.nvim", opts = { mapping = { "jk", "kj" } } },
  { "mbbill/undotree", cmd = { "UndotreeToggle", "UndotreeShow" } },
  { "romainl/vim-cool", event = "VeryLazy" },
  { "tpope/vim-fugitive", cmd = { "G", "Git" } },
  { "tpope/vim-rsi", event = "VeryLazy" },

  require "user.plugins.colorizer",
  require "user.plugins.colorschemes",
  require "user.plugins.harpoon",
  require "user.plugins.lsp",
  require "user.plugins.lualine",
  require "user.plugins.mini",
  require "user.plugins.ranger",
  require "user.plugins.telescope",
  require "user.plugins.treesitter",
  require "user.plugins.trouble",
  require "user.plugins.which-key",
})
