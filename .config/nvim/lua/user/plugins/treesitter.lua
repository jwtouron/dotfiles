return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    build = ":TSUpdate",
    config = function()
      require'nvim-treesitter.configs'.setup {
        -- A list of parser names, or "all" (the five listed parsers should always be installed)
        -- ensure_installed = { "c", "lua", "vim", "vimdoc", "query" },
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline", },

        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },

        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = '<c-space>',
            node_incremental = '<c-space>',
            scope_incremental = '<c-s>',
            node_decremental = '<m-space>',
          },
        },

        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ['aa'] = '@parameter.outer',
              ['ia'] = '@parameter.inner',
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
              ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
            },
            selection_modes = {
              ['@parameter.outer'] = 'v', -- charwise
              ['@function.outer'] = 'V', -- linewise
              ['@class.outer'] = '<c-v>', -- blockwise
            },
            include_surrounding_whitespace = true,
          },
          swap = {
            enable = true,
            swap_next = {
              ["<leader>a"] = "@parameter.inner",
            },
            swap_previous = {
              ["<leader>A"] = "@parameter.inner",
            },
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = { query = "@class.outer", desc = "Next class start" },
              ["]o"] = "@loop.*",
              ["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
              ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
            goto_next = {
              ["]d"] = "@conditional.outer",
            },
            goto_previous = {
              ["[d"] = "@conditional.outer",
            }
          },
        },

      }
    end
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    dependencies = "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    config = function()
      require'treesitter-context'.setup()
      vim.keymap.set("n", "[c", function()
        require("treesitter-context").go_to_context()
      end, { silent = true })
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
  },

  {
    "nvim-treesitter/playground",
    dependencies = "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    cmd = {
      "TSHighlightCapturesUnderCursor",
      "TSNodeUnderCursor",
      "TSPlaygroundToggle",
    },
    config = function()
      CreateFileTypeAutocmd("tsplayground", function()
        vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
      end)
    end,
  },
}
