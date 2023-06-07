local MyColorSchemes = vim.api.nvim_create_augroup("MyColorSchemes", { clear = true })

vim.api.nvim_create_autocmd("ColorScheme", {
  group = MyColorSchemes,
  callback = function()
    -- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    vim.cmd.highlight({
      "MatchParen",
      "term=underline",
      "cterm=underline",
      "gui=underline",
      "ctermbg=NONE",
      "guibg=NONE",
    })
  end,
})

local function colorscheme(name, spec)
  local ret = {
    name,
    -- lazy = true,
    priority = 1000,
  }
  for k, v in pairs(spec or {}) do
    ret[k] = v
  end
  return ret
end

local function create_colorscheme_autocmd(pattern, callback)
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = MyColorSchemes,
    pattern = pattern,
    callback = callback,
  })
end

local nord_spec = {
    config = function()
      create_colorscheme_autocmd("nord", function()
        vim.cmd.highlight({ "Normal", "guibg=#121212" })
        vim.cmd.highlight({ "SignColumn", "guibg=#121212" })
      end)
    end,
  }

local mountaineer_spec = {
    config = function()
      create_colorscheme_autocmd("mountaineer", function()
        vim.cmd.highlight({ "StatusLine", "guibg=#111111" })
        vim.cmd.highlight({ "Search", "guibg=#222222" })
      end)
    end,
  }

return {
  colorscheme("arcticicestudio/nord-vim", nord_spec),
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("ellisonleao/gruvbox.nvim", { opts = { contrast = "dark" } }),
  colorscheme("folke/tokyonight.nvim"),
  colorscheme("LunarVim/Colorschemes"),
  colorscheme("rose-pine/neovim"),
  colorscheme("TheNiteCoder/mountaineer.vim", mountaineer_spec),
  colorscheme("water-sucks/darkrose.nvim"),
}
