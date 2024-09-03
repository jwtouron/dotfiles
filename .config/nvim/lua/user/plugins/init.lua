local augroup = vim.api.nvim_create_augroup("user.plugins.init", { clear = true })

return {

  {
    "bronson/vim-visual-star-search",
  },

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
    'godlygeek/tabular',
    cmd = { "Tabularize", "Tab", },
  },

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

  {
    "lmburns/lf.nvim",
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

  {
    "mbbill/undotree",
    cmd = { "UndotreeShow", "UndotreeToggle" }
  },

  {
    "norcalli/nvim-colorizer.lua",
    name = "colorizer",
    init = function() vim.opt.termguicolors = true end,
    config = function() require("colorizer").setup() end,
  },

  {
    "nvim-zh/better-escape.vim",
    event = "InsertEnter",
    init = function()
      vim.g.better_escape_shortcut = {'jk', 'kj'}
      -- vim.g.better_escape_interval = 300
    end,
  },

  {
    'preservim/vim-markdown',
    dependencies = 'godlygeek/tabular',
    ft = "markdown",
    init = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_conceal_code_blocks = 0
      vim.g.vim_markdown_new_list_item_indent = 2
    end,
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
    event = "VeryLazy",
    init = function()
      vim.g.qf_auto_resize = 0
      vim.g.qf_max_height = 0
    end,
  },

  {
    "inkarkat/vim-redocommand",
    cmd = {
      "Redocommand", "R", "RedoRepeat", "RR", "RedoBufferRepeat", "RB", "RedoWindowRepeat", "RW"
    },
  },

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
