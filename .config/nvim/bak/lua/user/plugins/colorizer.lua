return {
  "norcalli/nvim-colorizer.lua",
  event = "VeryLazy",
  init = function()
    vim.opt.termguicolors = true
  end,
  config = function()
    require('colorizer').setup()
  end
}
