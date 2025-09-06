local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.api.nvim_create_autocmd("ColorScheme", {
  group = augroup,
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
  colorscheme("aktersnurra/no-clown-fiesta.nvim"),
  colorscheme("antonk52/lake.nvim"),
  colorscheme("blazkowolf/gruber-darker.nvim"),
  colorscheme("catppuccin/nvim", { name = 'catppuccin' }),
  colorscheme("datsfilipe/vesper.nvim"),
  colorscheme("dgox16/oldworld.nvim"),
  colorscheme("hardselius/warlock"),
  colorscheme("jnurmine/Zenburn"),
  colorscheme("kdheepak/monochrome.nvim"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("mcauley-penney/techbase.nvim"),
  colorscheme("mellow-theme/mellow.nvim"),
  colorscheme("miikanissi/modus-themes.nvim"),
  colorscheme("navarasu/onedark.nvim"),
  colorscheme("rafamadriz/neon"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("rjshkhr/shadow.nvim"),
  colorscheme("rose-pine/neovim", { name = "rose-pine" }),
  colorscheme("sainnhe/gruvbox-material", { init = function() vim.g.gruvbox_material_better_performance = 1 end }),
  colorscheme("scottmckendry/cyberdream.nvim"),
  colorscheme("webhooked/kanso.nvim"),
}
