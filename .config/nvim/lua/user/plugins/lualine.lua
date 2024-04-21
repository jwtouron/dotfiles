local function cwd()
  local path = vim.fn.fnamemodify(vim.fn.getcwd(), ":~")
  return path:gsub("(%.?[^/])[^/]*/", "%1/")
end

return {
  'nvim-lualine/lualine.nvim',
  enabled = false,
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  init = function() vim.opt.showmode = false end,
  opts = {
    options = {
      component_separators = { left = "·", right = "·" },
      section_separators = { left = "", right = "" },
    },
    sections = {
      lualine_b = {'diagnostics'},
      lualine_c = {
        cwd,
        {
          'filename',
          newfile_status = true,
          path = 1,
        }
      },
    },
  },
}
