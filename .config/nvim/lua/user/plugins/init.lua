return {

  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    config = function()
      require("better_escape").setup({mapping = {"jk", "kj"}})
    end,
  },

  {
    "mbbill/undotree",
    cmd = { "UndotreeShow", "UndotreeToggle" }
  },

  {
    "quick-history",
    dir = vim.fn.stdpath("config") .. '/lua/user/quick-history.nvim',
    cmd = "QuickHistory",
    keys = function()
      local make_callback = function(pat)
        return function() require('quick-history').open([[^\(.*|\)\? *\<]] .. pat .. [[\>]]) end
      end
      return {
        { "<leader>hd", make_callback([[cd]]), desc = "Quick History CD" },
        { "<leader>he", make_callback([[e\(dit\)\?]]), desc = "Quick History Edit" },
        { "<leader>hg", make_callback([[grep!\?]]), desc = "Quick History Grep" },
        { "<leader>hm", make_callback([[make\?]]), desc = "Quick History Make" },
      }
    end,
    config = true,
  },

  {
    "rlane/pounce.nvim",
    keys = {
      { "s", function() require'pounce'.pounce { } end, mode = { "n", "x" }, desc = "Pounce" },
      { "S", function() require'pounce'.pounce { do_repeat = true } end, desc = "Pounce Repeat" },
    },
  },

  {
    "romainl/vim-cool",
    event = "CmdlineEnter",
    config = function()
      vim.g.cool_total_matches = 1
    end
  },

  {
    "romainl/vim-qf",
    event = "QuickFixCmdPre",
    init = function()
      vim.g.qf_auto_resize = 0
      vim.g.qf_max_height = 0
    end,
  },

  {
    'stevearc/oil.nvim',
    event = "VeryLazy",
    opts = {},
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  {
    "tpope/vim-fugitive",
    cmd = { "G", "Git" },
  },

  {
    "tpope/vim-rsi",
    event = { "InsertEnter", "CmdlineEnter" },
    config = function()
      vim.keymap.set("n", "[<space>", function() vim.cmd [[normal! O]] end)
      vim.keymap.set("n", "]<space>", function() vim.cmd [[normal! o]] end)
    end,
  },

  {
    "tpope/vim-sleuth",
    event = "VeryLazy",
  },

}
