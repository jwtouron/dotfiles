vim.api.nvim_create_autocmd("ColorScheme", {
  group = MyAugroup,
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
    group = MyAugroup,
    pattern = pattern,
    callback = callback,
  })
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

local nord_spec = {
  config = function()
    create_colorscheme_autocmd("nord", function()
      vim.cmd.highlight({ "Normal", "guibg=#121212" })
      vim.cmd.highlight({ "SignColumn", "guibg=#121212" })
    end)
  end,
}

return {
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("Mofiqul/vscode.nvim"),
  colorscheme("catppuccin/nvim", { name = "catppuccin" }),
  colorscheme("cocopon/iceberg.vim"),
  colorscheme("folke/tokyonight.nvim"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("lunacookies/vim-substrata"),
  colorscheme("mcchrish/zenbones.nvim", { init = function() vim.g.bones_compat = true end }),
  colorscheme("nyoom-engineering/oxocarbon.nvim"),
  colorscheme("p00f/alabaster.nvim"),
  colorscheme("projekt0n/github-nvim-theme"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("rockerBOO/boo-colorscheme-nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim"),
  colorscheme("sainnhe/everforest", everforest_spec),
  colorscheme("sainnhe/gruvbox-material", gruvbox_material_spec),
  colorscheme("shaunsingh/nord.nvim", nord_spec),
  colorscheme("water-sucks/darkrose.nvim"),
  -- Monochrome
  colorscheme("aditya-azad/candle-grey"),
  colorscheme("andreypopp/vim-colors-plain"),
  colorscheme("axvr/photon.vim"),
  colorscheme("fxn/vim-monochrome"),
  colorscheme("hardselius/warlock"),
}
