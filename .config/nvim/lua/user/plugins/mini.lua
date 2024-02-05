local function mini(name, spec)
  local ret = {
    "echasnovski/mini." .. name,
    version = false,
  }
  for k, v in pairs(spec or { event = "VeryLazy", opts = {} }) do
    ret[k] = v
  end
  return ret
end

local bufremove_spec = {
  event = "CmdlineEnter",
  opts = {},
  init = function()
    vim.cmd.cabbr "bd lua require('mini.bufremove').delete()<Left>"
    vim.cmd.cabbr "bw lua require('mini.bufremove').wipeout()<Left>"
  end,
}

local trailspace_spec = {
  event = "VeryLazy",
  opts = {},
  init = function()
    vim.api.nvim_create_autocmd("ColorScheme", {
      group = MyAugroup,
      pattern = "*",
      command = "highlight MiniTrailspace guifg=salmon guisp=salmon gui=undercurl cterm=undercurl guibg=NONE ctermbg=NONE"
    })
  end,
}

return {
  mini('bracketed'),
  mini('bufremove', bufremove_spec),
  mini('comment'),
  mini('trailspace', trailspace_spec),
}
