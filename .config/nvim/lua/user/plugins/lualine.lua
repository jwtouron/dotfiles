local SHORTEN_PATH_REGEX = "(%.?[^/])[^/]*/"

local function shorten(path, func)
  if path then
    local basename = vim.fs.basename(path)
    if basename == '' then
      return ''
    else
      return func(path)
    end
  else
    return ''
  end
end

local function shorten_cwd(path)
  return shorten(path, function(p)
    return p:gsub(SHORTEN_PATH_REGEX, "%1/")
  end)
end

local function cwd()
  return shorten_cwd(vim.fn.getcwd())
end

local function shorten_filename(path)
  return shorten(path, function(p)
    local i, _ = p:find("[^/]*/[^/]*$")
    if not i then
      return p
    else
      local first = p:sub(1, i - 1)
      local second = p:sub(i)
      return first:gsub(SHORTEN_PATH_REGEX, "%1/") .. second
    end
    -- return p:gsub("(%.?[^/]?[^/]?[^/]?)[^/]*/", "%1/")  -- take 3 chars from each dir
  end)
end

local function filename()
  local data = shorten_filename(vim.fn.expand("%:~:."))
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
