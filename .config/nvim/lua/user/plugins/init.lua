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
        { '<leader>ee', function() exec.run() end },
        { '<leader>eh', function() exec.history() end },
        { '<leader>el', function() exec.show_last() end },
        { '<leader>er', function() exec.run_last() end },
      }
    end,
    config = true,
  },

  -- {{{1 godlygeek/tabular'

  {
    'godlygeek/tabular',
    cmd = { "Tabularize", "Tab", },
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

  -- {{{1 lmburns/lf.nvim

  {
    "lmburns/lf.nvim",
    enabled = false,
    dependencies = "akinsho/toggleterm.nvim",
    cmd = "Lf",
    opts = {
      border = "rounded",
      width = 9999,
      height = 9999,
      default_file_manager = true,
    },
    init = function() vim.g.lf_netrw = 1 end,
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

  -- {{{1 norcalli/nvim-colorizer.lua

  {
    "norcalli/nvim-colorizer.lua",
    name = "colorizer",
    init = function() vim.opt.termguicolors = true end,
    config = function() require("colorizer").setup() end,
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
    dependencies = "nvim-tree/nvim-web-devicons",
    opts = {
      show_icons = true,
      leader_key = '<leader>a', -- Recommended to be a single key
      buffer_leader_key = '<localleader>a', -- Per Buffer Mappings
    }
  },

  -- {{{1 preservim/vim-markdown

  {
    'preservim/vim-markdown',
    dependencies = 'godlygeek/tabular',
    ft = "markdown",
    init = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_conceal_code_blocks = 0
    end,
  },

  -- {{{1 quick-history

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
      vim.g.qf_auto_resize = 0
      vim.g.qf_max_height = 0
    end,
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

  -- {{{1 tpope/vim-fugitive

  {
    "tpope/vim-fugitive",
    cmd = { "G", "Git" },
  },

  -- {{{1 tpope/vim-rsi

  {
    "tpope/vim-rsi",
    event = { "InsertEnter", "CmdlineEnter" },
    keys = {
      { "[<space>", function() vim.cmd [[normal! O]] end },
      { "]<space>", function() vim.cmd [[normal! o]] end },
    },
  },

  -- {{{1 tpope/vim-sleuth

  {
    "tpope/vim-sleuth",
    event = "VeryLazy",
  },

}
