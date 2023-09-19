-- Actions that happen after EVERY colorscheme change.
vim.api.nvim_create_autocmd("ColorScheme", {
  group = MyAugroup,
  callback = function()
    -- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

    -- Matching parens are underlined, no BG change.
    vim.cmd.highlight({
      "MatchParen",
      "term=underline",
      "cterm=underline",
      "gui=underline",
      "ctermbg=NONE",
      "guibg=NONE",
    })

    -- Trailing whitespace is shown by salmon underdots.
    vim.cmd.highlight({
      "ExtraWhitespace",
      "guisp=#FA8072",
      "gui=underdotted",
      "ctermbg=NONE",
      "guibg=NONE"
    })
  end,
})

local colorschemes = {}

-- Trying to improve startup time. Not sure it's worth it.
vim.api.nvim_create_autocmd("VimEnter", {
  group = MyAugroup,
  callback = function()
    local i = 1
    local timer = vim.loop.new_timer()
    timer:start(0, 10, vim.schedule_wrap(function()
      if i <= #colorschemes then
        local colorscheme = colorschemes[i]
        local slash = colorscheme:find("/")
        if slash then
          colorscheme = colorscheme:sub(slash + 1)
        end
        vim.cmd.Lazy("load " .. colorscheme)
        i = i + 1
      else
        timer:stop()
      end
    end))
  end
})

local function colorscheme(name, spec)
  table.insert(colorschemes, (spec or {})['name'] or name)
  local ret = {
    name,
    -- event = "VeryLazy",
    lazy = true,
    -- priority = 1000,
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
  colorscheme("ishan9299/modus-theme-vim"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("lunacookies/vim-substrata"),
  colorscheme("mcchrish/zenbones.nvim", { init = function() vim.g.bones_compat = true end }),
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
  colorscheme("hardselius/warlock"),
  colorscheme("kdheepak/monochrome.nvim"),
}
