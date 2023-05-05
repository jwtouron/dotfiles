-- To set the colorscheme permanently, in lua/plugins/colorscheme.lua,
-- put something like:
-- return { { "LazyVim/LazyVim", opts = { colorscheme = "rose-pine" } } }

local Loader = require("lazy.core.loader")
local Util = require("lazy.util")

vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("mycolorscheme", { clear = true }),
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

local function colorscheme(name, opts)
  return {
    name,
    priority = 1000,
    config = function(plugin)
      if opts then
        local main = Loader.get_main(plugin)
        if main then
          require(main).setup(opts)
        else
          Util.error("Lua module not found for config of " .. plugin.name .. ".")
        end
      end
    end,
  }
end

return {
  colorscheme("arcticicestudio/nord-vim"),
  colorscheme("ellisonleao/gruvbox.nvim", { contrast = "dark" }),
  colorscheme("hardhackerlabs/theme-vim"),
  colorscheme("LunarVim/Colorschemes"),
  colorscheme("rose-pine/neovim"),
  colorscheme("water-sucks/darkrose.nvim"),
}
