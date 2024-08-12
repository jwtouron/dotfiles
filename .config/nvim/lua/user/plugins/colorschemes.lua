local augroup = vim.api.nvim_create_augroup("user.plugins.colorschemes", { clear = true })

vim.api.nvim_create_autocmd("ColorScheme", {
  group = augroup,
  pattern = "*",
  command = "highlight MatchParen term=underline cterm=underline gui=underline ctermbg=NONE guibg=NONE",
})

local function create_colorscheme_autocmd(pattern, callback)
  local opts = {
    group = augroup,
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

local everforest_spec = {
  config = function()
    vim.g.everforest_background = 'hard'
    vim.g.everforest_better_performance = 1
  end
}

local gruvbox_material_spec = {
  config = function()
    vim.g.gruvbox_material_background = 'hard'
    vim.g.gruvbox_material_better_performance = 1
  end
}

return {
  colorscheme("aktersnurra/no-clown-fiesta.nvim"),
  colorscheme("Aliqyan-21/darkvoid.nvim"),
  colorscheme("antonk52/lake.nvim"),
  colorscheme("blazkowolf/gruber-darker.nvim"),
  colorscheme("catppuccin/nvim", { name = 'catppuccin' }),
  colorscheme("cocopon/iceberg.vim"),
  colorscheme("dgox16/oldworld.nvim"),
  colorscheme("EdenEast/nightfox.nvim"),
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
  colorscheme("sainnhe/edge", { config = function() vim.g.edge_better_performance = 1 end }),
  colorscheme("sainnhe/everforest", everforest_spec),
  colorscheme("sainnhe/gruvbox-material", gruvbox_material_spec),
  colorscheme("sainnhe/sonokai", { config = function() vim.g.sonokai_better_performance = 1 end }),
  colorscheme("samharju/serene.nvim"),
  colorscheme("scottmckendry/cyberdream.nvim"),
  colorscheme("slugbyte/lackluster.nvim"),
  colorscheme("vague2k/vague.nvim"),
  colorscheme("water-sucks/darkrose.nvim"),
}
