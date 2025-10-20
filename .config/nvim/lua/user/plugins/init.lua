-- vim: set foldenable foldmethod=marker:

local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.cmd [[packadd cfilter]]
vim.cmd [[packadd nohlsearch]]

return {

  -- {{{1 exec

  {
    "exec",
    enabled = false,
    dir = vim.fn.stdpath("config") .. '/lua/user/exec.nvim',
    keys = function()
      local exec = require("exec")
      return {
        { '<leader>ee', exec.run, desc = "[E]xec Run" },
        { '<leader>eh', exec.show_history, desc = 'Exec Show [H]istory' },
        { '<leader>el', exec.show_last, desc = 'Exec Show [L]ast' },
        { '<leader>er', exec.rerun_last, desc = 'Exec [R]erun Last' },
      }
    end,
    config = true,
  },

  -- {{{1 folke/flash.nvim

  {
    "folke/flash.nvim",
    enabled = false,
    -- stylua: ignore
    keys = {
      { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
      { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
      { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
    },
    opts = {
      prompt = { enabled = false },
      highlight = { backdrop = false },
      modes = { char = { enabled = false, } },
    },
  },

  -- {{{1 jeetsukumaran/vim-indentwise

  {
    "jeetsukumaran/vim-indentwise",
    keys = {
      { "[-", "<Plug>(IndentWisePreviousLesserIndent)", mode = {"n", "x", "o"} },
      { "[=", "<Plug>(IndentWisePreviousEqualIndent)", mode = {"n", "x", "o"} },
      { "[+", "<Plug>(IndentWisePreviousGreaterIndent)", mode = {"n", "x", "o"} },
      { "]-", "<Plug>(IndentWiseNextLesserIndent)", mode = {"n", "x", "o"} },
      { "]=", "<Plug>(IndentWiseNextEqualIndent)", mode = {"n", "x", "o"} },
      { "]+", "<Plug>(IndentWiseNextGreaterIndent)", mode = {"n", "x", "o"} },
      { "[_", "<Plug>(IndentWisePreviousAbsoluteIndent)", mode = {"n", "x", "o"} },
      { "]_", "<Plug>(IndentWiseNextAbsoluteIndent)", mode = {"n", "x", "o"} },
      { "[%", "<Plug>(IndentWiseBlockScopeBoundaryBegin)", mode = {"n", "x", "o"} },
      { "]%", "<Plug>(IndentWiseBlockScopeBoundaryEnd)", mode = {"n", "x", "o"} },

      { "<c-h>", "<Plug>(IndentWisePreviousLesserIndent)", mode = {"n", "x", "o"} },
      { "<c-j>", "<Plug>(IndentWiseNextEqualIndent)", mode = {"n", "x", "o"} },
      { "<c-k>", "<Plug>(IndentWisePreviousEqualIndent)", mode = {"n", "x", "o"} },
      { "<c-l>", "<Plug>(IndentWiseNextGreaterIndent)", mode = {"n", "x", "o"} },
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
    cmd = "ColorizerToggle",
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
      { "s", function() require'pounce'.pounce { } end, mode = { "n", "x", "o" }, desc = "Pounce" },
      { "S", function() require'pounce'.pounce { do_repeat = true } end, desc = "Pounce Repeat" },
    },
    config = function()
      local hls = {
        PounceAccept = {
          fg = "#00CCFF",
        },
        PounceAcceptBest = {
          fg = "#CCFF00",
        },
        PounceGap = {
          link = "Search",
        },
        PounceMatch = {
          link = "Search",
        },
        PounceUnmatched = {
          fg = '#666666',
        },
      }

      local setup_hls = function()
        for hl, spec in pairs(hls) do
          vim.api.nvim_set_hl(0, hl, spec)
        end
      end

      setup_hls()

      vim.api.nvim_create_autocmd('Colorscheme', {
        group = augroup,
        callback = function() vim.schedule(setup_hls) end,
      })
    end,
  },

  -- {{{1 romainl/vim-cool

  {
    "romainl/vim-cool",
    enabled = false,
    event = "CmdlineEnter",
  },

  -- {{{1 romainl/vim-qf

  {
    "romainl/vim-qf",
    enabled = false,
    event = "QuickFixCmdPre",
    init = function()
      vim.g.qf_mapping_ack_style = 1
      vim.g.qf_auto_resize = 0
    end,
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

  -- {{{1 tpope/vim-repeat

  {
    "tpope/vim-repeat"
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
