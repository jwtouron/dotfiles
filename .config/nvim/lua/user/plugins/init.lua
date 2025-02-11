-- vim: set foldenable foldmethod=marker:

local augroup = vim.api.nvim_create_augroup("user.plugins.init", { clear = true })

return {

  -- {{{1 exec

  {
    "exec",
    dir = vim.fn.stdpath("config") .. '/lua/user/exec.nvim',
    keys = function()
      local exec = require("exec")
      return {
        { '<leader>ee', exec.run, desc = "Exec [R]un" },
        { '<leader>eh', exec.history, desc = 'Exec [H]istory' },
        { '<leader>el', exec.show_last, desc = 'Exec Show [L]ast' },
        { '<leader>er', exec.run_last, desc = 'Exec [R]un Last' },
      }
    end,
    config = true,
  },

  -- {{{1 jeetsukumaran/vim-indentwise

  {
    "jeetsukumaran/vim-indentwise",
    keys = {
      {"[-", mode = {"n", "x", "o"}},
      {"[=", mode = {"n", "x", "o"}},
      {"[+", mode = {"n", "x", "o"}},
      {"]-", mode = {"n", "x", "o"}},
      {"]=", mode = {"n", "x", "o"}},
      {"]+", mode = {"n", "x", "o"}},
      {"[_", mode = {"n", "x", "o"}},
      {"]_", mode = {"n", "x", "o"}},
      {"[%", mode = {"n", "x", "o"}},
      {"]%", mode = {"n", "x", "o"}},

      -- {"<c-h>", "<Plug>(IndentWisePreviousLesserIndent)", mode = {"n", "x", "o"}},
      -- {"<c-j>", "<Plug>(IndentWiseNextEqualIndent)", mode = {"n", "x", "o"}},
      -- {"<c-k>", "<Plug>(IndentWisePreviousEqualIndent)", mode = {"n", "x", "o"}},
      -- {"<c-l>", "<Plug>(IndentWiseNextGreaterIndent)", mode = {"n", "x", "o"}},
    },
  },

  -- {{{1 jwtouron/odin.vim

  {
    "jwtouron/odin.vim",
    ft = "odin",
    config = function()
      vim.api.nvim_create_autocmd({ "FileType" }, {
        group = augroup,
        pattern = "odin",
        command = "setlocal tabstop=8 noexpandtab errorformat=%f(%l:%c)\\ %m indentkeys+=<:>,0=},0=)",
      })
    end,
  },

  -- {{{1 max397574/better-escape.nvim

  {
    "max397574/better-escape.nvim",
    enabled = false,
    event = "InsertEnter",
    config = function()
      require("better_escape").setup {
        mappings = {
          i = {
            j = { k = "<Esc>", },
            k = { j = "<Esc>", },
          },
        }
      }
    end,
  },

  -- {{{1 mbbill/undotree

  {
    "mbbill/undotree",
    cmd = { "UndotreeShow", "UndotreeToggle" }
  },

  -- {{{1 michaeljsmith/vim-indent-object

  {
    "michaeljsmith/vim-indent-object",
    keys = {
      { "ai", nil, mode = { "o", "x" } },
      { "ii", nil, mode = { "o", "x" } },
      { "aI", nil, mode = { "o", "x" } },
      { "iI", nil, mode = { "o", "x" } },
    },
  },

  -- {{{1 norcalli/nvim-colorizer.lua

  {
    "norcalli/nvim-colorizer.lua",
    name = "colorizer",
    init = function() vim.opt.termguicolors = true end,
    config = function() require("colorizer").setup({}) end,
  },

  -- {{{1 nvim-zh/better-escape.vim

  {
    "nvim-zh/better-escape.vim",
    event = "InsertEnter",
    init = function()
      vim.g.better_escape_shortcut = {'jk', 'kj'}
      -- vim.g.better_escape_interval = 300
    end,
  },

  -- {{1 otavioschwanck/arrow.nvim

  {
    "otavioschwanck/arrow.nvim",
    enabled = false,
    dependencies = "nvim-tree/nvim-web-devicons",
    opts = {
      show_icons = true,
      leader_key = '<leader>a', -- Recommended to be a single key
      buffer_leader_key = '<localleader>a', -- Per Buffer Mappings
    }
  },

  -- {{{1 quick-history

  {
    "quick-history",
    enabled = false,
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

  -- {{{1 rlane/pounce.nvim

  {
    "rlane/pounce.nvim",
    keys = {
      { "s", function() require'pounce'.pounce { } end, mode = { "n", "x" }, desc = "Pounce" },
      { "S", function() require'pounce'.pounce { do_repeat = true } end, desc = "Pounce Repeat" },
    },
  },

  -- {{{1 romainl/vim-cool

  {
    "romainl/vim-cool",
    event = "CmdlineEnter",
  },

  -- {{{1 romainl/vim-qf

  {
    "romainl/vim-qf",
    event = "VeryLazy",
    init = function()
      vim.api.nvim_create_autocmd("QuickFixCmdPre", {
        pattern = "*",
        callback = function()
          vim.g.qf_max_height = math.floor(vim.o.lines / 2)
        end,
      })
    end,
    -- init = function()
    --   vim.g.qf_auto_resize = 0
    --   vim.g.qf_max_height = 0
    -- end,
  },

  -- {{{1 redo-command
  {
    "redo-command",
    dir = vim.fn.stdpath("config") .. '/lua/user/redo-command.nvim',
    cmd = { "RC", "RL", "RD" },
    keys =  {
      { "<leader>rr", "<cmd>RC<cr>", desc = "[R]edo Command" },
      { "<leader>rc", "<cmd>RC<cr>", desc = "[R]edo Command" },
      { "<leader>r1", "<cmd>1RC<cr>", desc = "[R]edo Command" },
      { "<leader>r2", "<cmd>2RC<cr>", desc = "[R]edo Command" },
      { "<leader>r3", "<cmd>3RC<cr>", desc = "[R]edo Command" },
      { "<leader>r4", "<cmd>4RC<cr>", desc = "[R]edo Command" },
      { "<leader>r5", "<cmd>5RC<cr>", desc = "[R]edo Command" },
      { "<leader>r6", "<cmd>6RC<cr>", desc = "[R]edo Command" },
      { "<leader>rl", "<cmd>RL<cr>", desc = "[L]ist Saved Commands" },
      { "<leader>rd", ":RD ", desc = "[D]elete Saved Commands" },
    },
    config = true,
  },

  -- {{{1 tpope/vim-fugitive

  {
    "tpope/vim-fugitive",
    cmd = { "G", "Git" },
  },

  -- {{{1 tpope/vim-rsi

  {
    "tpope/vim-rsi",
    event = { "InsertEnter", "CmdlineEnter" },
  },

  -- {{{1 tpope/vim-sleuth

  {
    "tpope/vim-sleuth",
    event = "VeryLazy",
  },

}
