return {
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = 'nvim-lua/plenary.nvim',
    keys = {
      { "<leader><space>", "<cmd>Telescope find_files<cr>", desc = "Search for files with Telescope" },
      { "<leader>,", "<cmd>Telescope buffers<cr>", desc = "Search for open buffers with Telescope" },
      { "<leader>]", "<cmd>Telescope tags<cr>", desc = "Search tags with Telescope" },

      { "<leader>tc", "<cmd>Telescope commands<cr>", desc = "Search commands with Telescope" },
      { "<leader>td", "<cmd>Telescope diagnostics<cr>", desc = "Search diagnostics with Telescope" },
      { "<leader>th", "<cmd>Telescope help_tags<cr>", desc = "Search help tags with Telescope" },
      { "<leader>tk", "<cmd>Telescope keymaps<cr>", desc = "Search keymaps with Telescope" },
      { "<leader>tl", "<cmd>Telescope loclist<cr>", desc = "Search loclist with Telescope" },
      { "<leader>tm", "<cmd>Telescope marks<cr>", desc = "Search marks with Telescope" },
      { "<leader>tM", "<cmd>Telescope man_pages<cr>", desc = "Search man pages with Telescope" },
      { "<leader>to", "<cmd>Telescope oldfiles<cr>", desc = "Search oldfiles with Telescope" },
      { "<leader>tq", "<cmd>Telescope quickfix<cr>", desc = "Search quickfix with Telescope" },
      { "<leader>tr", "<cmd>Telescope registers<cr>", desc = "Search registers with Telescope" },
      { "<leader>tR", "<cmd>Telescope resume<cr>", desc = "Resume last Telescope search" },
    },
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    dependencies = "nvim-telescope/telescope.nvim",
    build = 'make',
    event = "VeryLazy",
    config = function()
      require('telescope').load_extension('fzf')
    end,
  },

  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    keys = {
      { "<leader>tg", "<cmd>lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>" },
    },
    config = function()
      require("telescope").load_extension("live_grep_args")
    end,
  },

  {
    "debugloop/telescope-undo.nvim",
    config = function()
      require("telescope").load_extension("undo")
    end,
  },
}
