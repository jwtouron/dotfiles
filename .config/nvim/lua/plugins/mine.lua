return {
  -- LazyVim Tweaks
  --
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
  { "nvim-pack/nvim-spectre", enabled = false },
  { "rcarriga/nvim-notify", enabled = false },

  -- New plugins
  --
  { "AckslD/muren.nvim", config = true, },
  { "ixru/nvim-markdown", ft = "markdown" },
  { "max397574/better-escape.nvim", opts = { mapping = { "jk", "kj" } } },
  { "mbbill/undotree", cmd = { "UndotreeShow", "UndotreeToggle" } },
  {
    "norcalli/nvim-colorizer.lua",
    event = "VeryLazy",
    config = function()
      require("colorizer").setup()
    end,
  },
  { "romainl/vim-cool", event = "VeryLazy" },
  { "tpope/vim-fugitive", cmd = { "G", "Git", "Ggrep" } },
  { "tpope/vim-rsi", event = "VeryLazy" },
}
