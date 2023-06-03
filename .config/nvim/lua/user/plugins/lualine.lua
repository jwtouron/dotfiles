return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons', },
  event = "VeryLazy",
  init = function() vim.opt.showmode = false end,
  opts = {
    options = {
      component_separators = { left = "·", right = "·" },
      section_separators = { left = "", right = "" },
    },
  },
}
