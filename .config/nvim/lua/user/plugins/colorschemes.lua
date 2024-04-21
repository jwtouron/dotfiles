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

local gruvbox_material_spec = {
  config = function()
    vim.g.gruvbox_material_background = 'hard'
    vim.g.gruvbox_material_better_performance = 1
  end
}

local phoenix_spec = {
  init = function()
    create_colorscheme_autocmd(
      "phoenix",
      "hi Identifier guifg=#999999 guibg=NONE gui=NONE ctermfg=246"
    )
  end
}

local tokyonight_spec = {
  opts = {
    on_highlights = function(hl)
      hl.Error = { bg = nil }
      hl.Todo = { bg = nil }
    end,
  },
}

return {
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("aktersnurra/no-clown-fiesta.nvim"),
  colorscheme("folke/tokyonight.nvim", tokyonight_spec),
  colorscheme("jnurmine/Zenburn"),
  colorscheme("mcchrish/zenbones.nvim", { config = function() vim.g.bones_compat = true end }),
  colorscheme("mellow-theme/mellow.nvim"),
  colorscheme("navarasu/onedark.nvim"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("rmehri01/onenord.nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim", { name = "rose-pine" }),
  colorscheme("sainnhe/gruvbox-material", gruvbox_material_spec),
  -- Simple Colorschemes
  colorscheme("hardselius/warlock"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("widatama/vim-phoenix", phoenix_spec),
}
