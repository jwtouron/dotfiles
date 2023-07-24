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

local gruvbox_baby_spec = {
  init = function()
    vim.g.gruvbox_baby_background_color = 'dark'
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
  colorscheme("2nthony/vitesse.nvim", { dependencies = { "tjdevries/colorbuddy.nvim" }, }),
  colorscheme("bluz71/vim-moonfly-colors"),
  colorscheme("cocopon/iceberg.vim"),
  colorscheme("EdenEast/nightfox.nvim"),
  colorscheme("folke/tokyonight.nvim"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("luisiacc/gruvbox-baby", gruvbox_baby_spec),
  colorscheme("lunacookies/vim-substrata"),
  colorscheme("nyoom-engineering/oxocarbon.nvim"),
  colorscheme("p00f/alabaster.nvim"),
  colorscheme("projekt0n/github-nvim-theme"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("rockerBOO/boo-colorscheme-nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim"),
  colorscheme("shaunsingh/nord.nvim", nord_spec),
  colorscheme("water-sucks/darkrose.nvim"),
}
