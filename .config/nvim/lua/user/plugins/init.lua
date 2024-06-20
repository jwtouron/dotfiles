return {

  {
    "exec",
    dir = vim.fn.stdpath("config") .. '/lua/user/exec.nvim',
    cmd = { "Exec", "ExecHistory", "ExecLastCommand" },
    keys = {
      { "<leader>ee", function() require('exec').exec_last_command() end, desc = "Exec last command" },
      { "<leader>eh", function() require('exec').exec_history() end, desc = "Exec history" },
      { "<leader>eo", function() require('exec').toggle_output() end, desc = "Toggle open Exec output" },
    },
    config = true,
  },

  {
    "jeetsukumaran/vim-indentwise",
    enabled = false,
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

  {
    "jessekelighine/vindent.vim",
    keys = {
      { '[=', mode = { 'n', 'x', 'o' } },
      { ']=', mode = { 'n', 'x', 'o' } },
      { '[+', mode = { 'n', 'x', 'o' } },
      { ']+', mode = { 'n', 'x', 'o' } },
      { '[-', mode = { 'n', 'x', 'o' } },
      { ']-', mode = { 'n', 'x', 'o' } },
      { '[;', mode = { 'n', 'x', 'o' } },
      { '];', mode = { 'n', 'x', 'o' } },
      { '[p', mode = { 'n', 'x', 'o' } },
      { ']p', mode = { 'n', 'x', 'o' } },
      { 'ii', mode = { 'n', 'x', 'o' } },
      { 'ai', mode = { 'n', 'x', 'o' } },
      { 'aI', mode = { 'n', 'x', 'o' } },
    },
    init = function()
      vim.g.vindent_motion_OO_prev   = '[='
      vim.g.vindent_motion_OO_next   = ']='
      vim.g.vindent_motion_more_prev = '[+'
      vim.g.vindent_motion_more_next = ']+'
      vim.g.vindent_motion_less_prev = '[-'
      vim.g.vindent_motion_less_next = ']-'
      vim.g.vindent_motion_diff_prev = '[;'
      vim.g.vindent_motion_diff_next = '];'
      vim.g.vindent_motion_XX_ss     = '[p'
      vim.g.vindent_motion_XX_se     = ']p'
      vim.g.vindent_object_XX_ii     = 'ii'
      vim.g.vindent_object_XX_ai     = 'ai'
      vim.g.vindent_object_XX_aI     = 'aI'
      vim.g.vindent_infer          = 1
    end
  },

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
    "tpope/vim-fugitive",
    cmd = { "G", "Git" },
  },

  {
    "tpope/vim-rsi",
    event = { "InsertEnter", "CmdlineEnter" },
    keys = {
      { "[<space>", function() vim.cmd [[normal! O]] end },
      { "]<space>", function() vim.cmd [[normal! o]] end },
    },
  },

  {
    "tpope/vim-sleuth",
    event = "VeryLazy",
  },

}
