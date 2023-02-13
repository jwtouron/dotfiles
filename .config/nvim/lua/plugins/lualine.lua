local function config()
  require('lualine').setup {
    options = {
      component_separators = { left = '·', right = '·' },
      section_separators = { left = '', right = '' },
    }
  }
end

return {
  'nvim-lualine/lualine.nvim',
  dependencies = 'kyazdani42/nvim-web-devicons',
  config = config,
}
