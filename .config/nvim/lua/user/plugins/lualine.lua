local function shorten_path(path)
  if path then
    local basename = vim.fs.basename(path)
    if basename == '' then
      return ''
    else
      return path:gsub("(%.?[^/])[^/]*/", "%1/")
    end
  end
  return ''
end

local function cwd()
  return shorten_path(vim.fn.getcwd())
end

local function filename()
  local data = shorten_path(vim.fn.expand("%:~:."))
  if data == '' then data = "[No Name]" end
  local symbols = {}
  if vim.bo.modified then
    table.insert(symbols, "[+]")
  end
  if vim.bo.modifiable == false or vim.bo.readonly == true then
    table.insert(symbols, "[-]")
  end
  return data .. (#symbols > 0 and ' ' .. table.concat(symbols, '') or '')
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
      lualine_c = { cwd, filename },
    },
  },
}
