local function shorten_path(path)
  if path then
    return path:gsub("(%.?[^/])[^/]*/", "%1/")
  end
  return ''
end

local cwd = ''

local function get_cwd()
  return cwd
end

local function update_cwd()
  cwd = shorten_path(vim.fn.getcwd())
end

return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons", },
  event = "VeryLazy",
  init = function() vim.opt.showmode = false end,
  opts = {
    options = {
      component_separators = { left = "·", right = "·" },
      section_separators = { left = "", right = "" },
    },
    sections = {
      lualine_b = { "diagnostics" },
      lualine_c = { get_cwd, "filename" },
    },
  },
  config = function(_, opts)
    update_cwd()

    vim.api.nvim_create_autocmd("DirChanged", {
      group = vim.api.nvim_create_augroup("MyLualine", { clear = true }),
      pattern = "*",
      callback = function()
        update_cwd()
      end,
    })

    require("lualine").setup(opts)
  end,
}
