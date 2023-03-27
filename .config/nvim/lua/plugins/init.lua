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

local function setup(plugin, opts)
  return function()
    require(plugin).setup(opts)
  end
end

require("lazy").setup({
  require "plugins.harpoon",
  require "plugins.lualine",
  require "plugins.mini",
  require "plugins.telescope",

  { 'jakewvincent/mkdnflow.nvim', config = function() require('mkdnflow').setup() end },
  { 'max397574/better-escape.nvim', config = setup('better_escape', { mapping = { "jk", "kj" } }) },
  { 'mbbill/undotree' },
  { 'norcalli/nvim-colorizer.lua', init = function() vim.o.termguicolors = true end, config = setup('colorizer') },
  { 'romainl/vim-cool' },
  { "tpope/vim-rsi" },
  { "tpope/vim-sleuth" },

  { 'folke/tokyonight.nvim', config = setup('tokyonight', { dim_inactive = false }) },
  { 'LunarVim/Colorschemes' },
  { 'rose-pine/neovim', config = setup("rose-pine", {disable_background = true}) },
  require "plugins.everforest",
})
