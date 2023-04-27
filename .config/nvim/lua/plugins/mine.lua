local function config(plugin, opts)
  return function()
    require(plugin).setup(opts)
  end
end

return {
  -- LazyVim Tweaks
  --
  { "goolord/alpha-nvim", enabled = false },
  { "neovim/nvim-lspconfig", opts = { autoformat = false } },
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = function(_, opts)
      opts.options.component_separators = { left = "·", right = "·" }
      opts.options.section_separators = { left = "", right = "" }
    end,
  },

  -- New Plugins
  --
  {
    "max397574/better-escape.nvim",
    config = config("better_escape", { mapping = { "jk", "kj" } }),
  },
  { "mbbill/undotree" },
  {
    "norcalli/nvim-colorizer.lua",
    init = function()
      vim.o.termguicolors = true
    end,
    config = config("colorizer"),
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    keys = {
      {
        "<leader>sB",
        ":Telescope file_browser path=%:p:h=%:p:h<cr>",
        desc = "Browse Files",
      },
    },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },
  { "romainl/vim-cool" },
  { "tpope/vim-fugitive" },
  { "tpope/vim-rsi" },
}
