return {
  -- LazyVim Tweaks
  --
  { "folke/lazy.nvim", checker = { enabled = false } },
  { "folke/noice.nvim", enabled = false },
  { "goolord/alpha-nvim", enabled = false },
  {
    "hrsh7th/nvim-cmp",
    dependencies = { "hrsh7th/cmp-nvim-lua" },
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, { { name = "nvim_lua" } }))
    end,
  },
  { "lewis6991/gitsigns.nvim", enabled = false },
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
  { "rcarriga/nvim-notify", enabled = false },

  -- New plugins
  --
  { "ixru/nvim-markdown", ft = "markdwon" },
  { "max397574/better-escape.nvim", opts = { mapping = { "jk", "kj" } } },
  { "mbbill/undotree", cmd = { "UndotreeToggle", "UndotreeShow" } },
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
  },
  { "romainl/vim-cool" },
  { "tpope/vim-fugitive", cmd = { "Git", "Ggrep" } },
  { "tpope/vim-rsi" },
}
