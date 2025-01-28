---@class TextObject
---@field name string
---@field select_keymap string | string[]
---@field goto_next_start string | nil
---@field goto_next_end string | nil
---@field goto_previous_start string | nil
---@field goto_previous_end string | nil

---@type TextObject[]
local text_objects = {
  {
    name = "@block.outer",
    select_keymap = "ab",
  },

  {
    name = "@block.inner",
    select_keymap = "ib",
  },

  {
    name = "@class.outer",
    select_keymap = "ac",
    goto_next_start = "]]",
    goto_next_end = "][",
    goto_previous_start = "[[",
    goto_previous_end = "[]",
  },

  {
    name = "@class.inner",
    select_keymap = "ic",
  },

  {
    name = "@function.outer",
    select_keymap = "af",
    goto_next_start = "]m",
    goto_next_end = "]M",
    goto_previous_start = "[m",
    goto_previous_end = "[M",
  },

  {
    name = "@function.inner",
    select_keymap = "if",
  },

  {
    name = "@parameter.outer",
    select_keymap = "aa",
    selection_mode = "v",
  },

  {
    name = "@parameter.inner",
    select_keymap = "ia",
    selection_mode = "v",
  },
}

return {
  {
    'nvim-treesitter/nvim-treesitter',
    keys = {
      { "<c-=>", nil, mode = { "n", "o", "x" } },
    },
    config = function()
      local ensure_installed = {
        "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline",  -- MUST always be installed
        "go", "python",  -- Additional langauges
      }
      require("nvim-treesitter").setup {
        ensure_installed = ensure_installed,
        sync_install = false,
        auto_install = true,
      }

      require("nvim-treesitter.configs").setup {
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<c-=>",
            node_incremental = "<c-=>",
            scope_incremental = false,
            node_decremental = "<c-->",
          },
        },
      }
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    event = "VeryLazy",
    dependences = "nvim-treesitter/nvim-treesitter",
    -- keys = function()
    --   local keys = {}
    --
    --   for _, to in ipairs(text_objects) do
    --     local select_keymap = to.select_keymap
    --     if type(select_keymap) == 'string' then
    --       select_keymap = { select_keymap }
    --     end
    --     for _, km in ipairs(select_keymap) do
    --       table.insert(keys, { km, nil, mode = { "o", "x", } })
    --     end
    --
    --     for _, f in ipairs({ "goto_next_start", "goto_next_end", "goto_previous_start", "goto_previous_end" }) do
    --       if to[f] then
    --         table.insert(keys, { to[f], nil, mode = { "n", "x" } })
    --       end
    --     end
    --   end
    --
    --   return keys
    -- end,
    config = function()
      local keymaps = {}
      local goto_next_start = {}
      local goto_next_end = {}
      local goto_previous_start = {}
      local goto_previous_end = {}

      for _, to in ipairs(text_objects) do
        local select_keymap = to.select_keymap
        if type(select_keymap) == 'string' then
          select_keymap = { select_keymap }
        end
        for _, km in ipairs(select_keymap) do
          keymaps[km] = to.name
        end

        if to.goto_next_start     then goto_next_start[to.goto_next_start] = to.name         end
        if to.goto_next_end       then goto_next_end[to.goto_next_end] = to.name             end
        if to.goto_previous_start then goto_previous_start[to.goto_previous_start] = to.name end
        if to.goto_previous_end   then goto_previous_end[to.goto_previous_end] = to.name     end
      end

      require('nvim-treesitter.configs').setup {
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = keymaps,
            -- include_surrounding_whitespace = true,
          },

          move = {
            enable = true,
            goto_next_start = goto_next_start,
            goto_next_end = goto_next_end,
            goto_previous_start = goto_previous_start,
            goto_previous_end = goto_previous_end,
          },
        }
      }
    end,
  },
}
