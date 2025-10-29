return {
  {
    "nvim-treesitter/nvim-treesitter",
    enabled = false,
    event = "FileType",
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline" },
        sync_install = false,
        auto_install = false,

        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-=>",
            node_incremental = "<C-=>",
            scope_incremental = false,
            node_decremental = "<C-->",
          },
        },

      }
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    enabled = false,
    dependencies = "nvim-treesitter/nvim-treesitter",
    event = "FileType",
    config = function()
      if true then
        local x = 1
      end
      require'nvim-treesitter.configs'.setup {
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
              ["ab"] = "@block.outer",
              ["ib"] = "@block.inner",
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
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = { query = "@class.outer", desc = "Next class start" },
              ["]b"] = "@block.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
              ["]B"] = "@block.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
              ["[b"] = "@block.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
              ["[B"] = "@block.outer",
            },
          },
        },
      }
    end,
  },
}
