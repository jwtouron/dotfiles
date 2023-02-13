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
  require "plugins.lualine",
  require "plugins.telescope",
  require "plugins.harpoon",

  { 'bronson/vim-trailing-whitespace' },
  { 'max397574/better-escape.nvim', config = setup('better_escape', { mapping = { "jk", "kj" } }) },
  { 'mbbill/undotree' },
  { 'norcalli/nvim-colorizer.lua', init = function() vim.o.termguicolors = true end, config = setup('colorizer') },
  { 'numToStr/Comment.nvim', config = setup('Comment') },
  { 'romainl/vim-cool' },
  { "tpope/vim-rsi" },
  { "tpope/vim-sleuth" },
  { 'tpope/vim-surround' },
  { "tpope/vim-unimpaired" },

  { 'LunarVim/Colorschemes' },
  { 'rose-pine/neovim', config = function() require("rose-pine").setup({disable_background = true}) end },
})
