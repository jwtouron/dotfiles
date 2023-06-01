-- Tweaks to LazyVim plugins

local function disable(plugin)
  return { plugin, enabled = false }
end

return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = { "hrsh7th/cmp-nvim-lua" },
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, { { name = "nvim_lua" } }))
    end,
  },
  { "neovim/nvim-lspconfig", opts = { autoformat = false } },
  {
    "nvim-lualine/lualine.nvim",
    opts = {
      options = {
        component_separators = { left = "·", right = "·" },
        section_separators = { left = "", right = "" },
      },
    },
  },

  -- disable "echasnovski/mini.surround",
  disable "folke/noice.nvim",
  -- disable "ggandor/flit.nvim",
  -- disable "ggandor/leap.nvim",
  disable "goolord/alpha-nvim",
  disable "lewis6991/gitsigns.nvim",
  disable "nvim-pack/nvim-spectre",
  disable "rcarriga/nvim-notify",
}
