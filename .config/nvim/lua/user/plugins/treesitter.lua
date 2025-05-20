local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

---@class TextObject
---@field name string
---@field select_keymap string
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
  },

  {
    name = "@parameter.inner",
    select_keymap = "ia",
  },
}

return {
  {
    'nvim-treesitter/nvim-treesitter',
    cond = function() return vim.fn.has('macunix') ~= 1 end,
    event = "FileType",
    -- keys = {
    --   { "<c-=>", nil, mode = { "n", "o", "x" } },
    -- },
    config = function()
      local ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline" }
      require("nvim-treesitter.configs").setup {
        ensure_installed = ensure_installed,
        sync_install = false,
        auto_install = false,

        -- incremental_selection = {
        --   enable = true,
        --   keymaps = {
        --     init_selection = "<c-=>",
        --     node_incremental = "<c-=>",
        --     scope_incremental = false,
        --     node_decremental = "<c-->",
        --   },
        -- },
      }
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    enabled = false,
    cond = function() return vim.fn.has('macunix') ~= 1 end,
    dependences = "nvim-treesitter/nvim-treesitter",
    event = "FileType",
    init = function()
      local needs_reparse = {}

      vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "TextChangedP" }, {
        group = augroup,
        pattern = '*',
        callback = function()
          needs_reparse[vim.fn.bufnr()] = true
        end
      })

      vim.api.nvim_create_autocmd("FileType", {
        group = augroup,
        pattern = '*',
        callback = function()
          local ok, parser = pcall(vim.treesitter.get_parser)

          if ok then
            local select = require('nvim-treesitter.textobjects.select')
            local move = require('nvim-treesitter.textobjects.move')
            local bufnr = vim.fn.bufnr()

            local check_reparse = function()
              if needs_reparse[bufnr] then
                parser:parse()
                needs_reparse[bufnr] = nil
              end
            end

            for _, to in ipairs(text_objects) do
              if to.select_keymap then
                for _, op in ipairs({ "o", "x" }) do
                  vim.keymap.set(op, to.select_keymap, function()
                    check_reparse()
                    select.select_textobject(to.name, nil, op)
                  end, { buffer = bufnr })
                end
              end

              for _, f in ipairs({ 'goto_next_start', 'goto_next_end', 'goto_previous_start', 'goto_previous_end' }) do
                if to[f] then
                  vim.keymap.set({ "n", "x" }, to[f], function()
                    check_reparse()
                    move[f](to.name)
                  end, { buffer = bufnr })
                end
              end
            end
          end
        end,
      })
    end,
    config = function()

      require('nvim-treesitter.configs').setup {
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
          },

          move = {
            enable = true,
          },
        }
      }
    end,
  },
}
