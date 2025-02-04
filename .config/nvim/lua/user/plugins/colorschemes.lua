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

return {
  colorscheme("akinsho/horizon.nvim", { version = "*" }),
  colorscheme("aktersnurra/no-clown-fiesta.nvim"),
  colorscheme("AlexvZyl/nordic.nvim"),
  colorscheme("antonk52/lake.nvim"),
  colorscheme("blazkowolf/gruber-darker.nvim"),
  colorscheme("catppuccin/nvim", { name = 'catppuccin' }),
  colorscheme("cpwrs/americano.nvim"),
  colorscheme("datsfilipe/vesper.nvim"),
  colorscheme("dgox16/oldworld.nvim"),
  colorscheme("drewxs/ash.nvim"),
  colorscheme("ficcdaf/ashen.nvim"),
  colorscheme("folke/tokyonight.nvim"),
  colorscheme("hardselius/warlock"),
  colorscheme("jacoborus/tender.vim"),
  colorscheme("jnurmine/Zenburn"),
  colorscheme("kdheepak/monochrome.nvim"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("mellow-theme/mellow.nvim"),
  colorscheme("miikanissi/modus-themes.nvim"),
  colorscheme("navarasu/onedark.nvim"),
  colorscheme("nyoom-engineering/oxocarbon.nvim"),
  colorscheme("oonamo/ef-themes.nvim"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim", { name = "rose-pine" }),
  colorscheme("sainnhe/edge", { init = function() vim.g.edge_better_performance = 1 end }),
  colorscheme("sainnhe/gruvbox-material", { init = function() vim.g.gruvbox_material_better_performance = 1 end }),
  colorscheme("sainnhe/sonokai", { init = function() vim.g.sonokai_better_performance = 1 end }),
  colorscheme("samharju/serene.nvim"),
  colorscheme("scottmckendry/cyberdream.nvim"),
  colorscheme("slugbyte/lackluster.nvim"),
  colorscheme("vague2k/vague.nvim"),
  colorscheme("water-sucks/darkrose.nvim"),
}
