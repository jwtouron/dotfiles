local user_color_scheme_group = vim.api.nvim_create_augroup("UserColorScheme", { clear = true })

vim.api.nvim_create_autocmd("ColorScheme", {
  group = user_color_scheme_group,
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
  local result = {
    name,
    lazy = true,
    priority = 1000,
  }
  for k, v in pairs(spec or {}) do
    result[k] = v
  end
  return result
end

local function create_colorscheme_autocmd(pattern, callback)
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = user_color_scheme_group,
    pattern = pattern,
    callback = callback,
  })
end

return {
  colorscheme("arcticicestudio/nord-vim", {
    config = function()
      create_colorscheme_autocmd("nord", function() vim.cmd.highlight({ "Normal", "guibg=#121212" }) end)
    end
  }),
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("ellisonleao/gruvbox.nvim", { opts = { contrast = "dark" } }),
  colorscheme("LunarVim/Colorschemes"),
  colorscheme("rose-pine/neovim"),
  colorscheme("water-sucks/darkrose.nvim"),
}
