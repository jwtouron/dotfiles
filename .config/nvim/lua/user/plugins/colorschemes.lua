vim.api.nvim_create_autocmd("ColorScheme", {
  group = MyAugroup,
  pattern = "*",
  command = "highlight MatchParen term=underline cterm=underline gui=underline ctermbg=NONE guibg=NONE",
})

local function create_colorscheme_autocmd(pattern, callback)
  local opts = {
    group = MyAugroup,
    pattern = pattern,
  }
  if type(callback) == 'function' then
    opts.callback = callback
  else
    opts.command = callback
  end
  vim.api.nvim_create_autocmd("ColorScheme", opts)
end

local function colorscheme(name, spec)
  local ret = {
    name,
    lazy = true,
  }
  for k, v in pairs(spec or {}) do
    ret[k] = v
  end
  return ret
end

local phoenix_spec = {
  init = function()
    create_colorscheme_autocmd(
      "phoenix",
      "hi Identifier guifg=#999999 guibg=NONE gui=NONE ctermfg=246"
    )
  end
}

return {
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("mcchrish/zenbones.nvim", { init = function() vim.g.bones_compat = true end }),
  colorscheme("mellow-theme/mellow.nvim"),
  colorscheme("navarasu/onedark.nvim"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim"),
  colorscheme("sainnhe/gruvbox-material", { init = function() vim.g.gruvbox_material_background = 'hard' end }),
  -- Simple Colorschemes
  colorscheme("hardselius/warlock"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("widatama/vim-phoenix", phoenix_spec),
}
