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

local everforest_material_spec = {
  config = function()
    vim.g.everforest_material_background = 'hard'
    vim.g.everforest_material_better_performance = 1
  end
}

local gruvbox_material_spec = {
  config = function()
    vim.g.gruvbox_material_background = 'hard'
    vim.g.gruvbox_material_better_performance = 1
  end
}

return {
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("aktersnurra/no-clown-fiesta.nvim"),
  colorscheme("catppuccin/nvim", { name = 'catppuccin' }),
  colorscheme("cocopon/iceberg.vim"),
  colorscheme("dgox16/oldworld.nvim"),
  colorscheme("folke/tokyonight.nvim"),
  colorscheme("hardselius/warlock"),
  colorscheme("jacoborus/tender.vim"),
  colorscheme("jnurmine/Zenburn"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("kvrohit/substrata.nvim"),
  colorscheme("mcchrish/zenbones.nvim", { config = function() vim.g.bones_compat = true end }),
  colorscheme("mellow-theme/mellow.nvim"),
  colorscheme("miikanissi/modus-themes.nvim"),
  colorscheme("navarasu/onedark.nvim"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("rmehri01/onenord.nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim", { name = "rose-pine" }),
  colorscheme("sainnhe/everforest", everforest_material_spec),
  colorscheme("sainnhe/gruvbox-material", gruvbox_material_spec),
  colorscheme("sainnhe/sonokai", { config = function() vim.g.sonokai_material_better_performance = 1 end }),
  colorscheme("water-sucks/darkrose.nvim"),
}
