local Util = require("user.util")

local keys = {
  { "<leader><leader>", function(builtin) builtin.find_files({ cwd = Util.get_root() }) end, "Search Files (root dir)" },
  { "<leader>,", function(builtin) builtin.buffers() end, "Search Buffers" },
  { "<leader>/", function(builtin) builtin.current_buffer_fuzzy_find() end, "Current Buffer Fuzzy Find" },

  { "<leader>sb", function(builtin) builtin.buffers() end, "[B]uffers" },
  { "<leader>sc", function(builtin) builtin.commands() end, "[C]ommands" },
  { "<leader>sC", function(builtin) builtin.command_history() end, "[C]ommand History" },
  { "<leader>sd", function(builtin) builtin.diagnostics({ bufnr = 0}) end, "[D]iagnostics (current buffer)" },
  { "<leader>sD", function(builtin) builtin.diagnostics({ bufnr = nil}) end, "[D]iagnostics (all buffers)" },
  { "<leader>sf", function(builtin) builtin.find_files({ cwd = Util.get_root() }) end, "[F]iles (root dir)" },
  { "<leader>sF", function(builtin) builtin.find_files() end, "[F]iles (cwd)" },
  { "<leader>sh", function(builtin) builtin.help_tags() end, "[H]elp" },
  { "<leader>sk", function(builtin) builtin.keymaps() end, "[K]eymaps" },
  { "<leader>sl", function(builtin) builtin.loclist() end, "[L]oclist" },
  { "<leader>sm", function(builtin) builtin.marks() end, "[M]arks" },
  { "<leader>sM", function(builtin) builtin.man_pages() end, "[M]an Pages" },
  { "<leader>so", function(builtin) builtin.oldfiles() end, "[O]ldfiles" },
  { "<leader>sq", function(builtin) builtin.quickfix() end, "[Q]uickfix" },
  { "<leader>sQ", function(builtin) builtin.quickfixhistory() end, "[Q]uickfix History" },
  { "<leader>sr", function(builtin) builtin.registers() end, "[R]egisters" },
  { "<leader>sR", function(builtin) builtin.resume() end, "[R]esume" },
  { "<leader>ss", function(builtin) builtin.spell_suggest() end, "[S]pell Suggestions" },
  { "<leader>sS", function(builtin) builtin.colorscheme({ enable_preview = true }) end, "Color[S]chemes" },

  -- LSP
  { "<leader>sLc", function(builtin) builtin.lsp_incoming_calls() end, "[L]SP Incoming [C]alls" },
  { "<leader>sLC", function(builtin) builtin.lsp_outgoing_calls() end, "[L]SP Outgoing [C]alls" },
  { "<leader>sLd", function(builtin) builtin.lsp_definitions() end, "[L]SP [D]efinitions" },
  { "<leader>sLi", function(builtin) builtin.lsp_implementations() end, "[L]SP [I]mplementations" },
  { "<leader>sLr", function(builtin) builtin.lsp_references() end, "[L]SP [R]eferences" },
  { "<leader>sLs", function(builtin) builtin.lsp_document_symbols() end, "[L]SP Document [S]ymbols" },
  { "<leader>sLS", function(builtin) builtin.lsp_workspace_symbols() end, "[L]SP Workspace [S]ymbols" },
  { "<leader>sLt", function(builtin) builtin.lsp_type_definitions() end, "[L]SP [T]ype Definitions" },
}

return {
  {
    "nvim-telescope/telescope.nvim",
    version = "*",
    dependencies = { "nvim-lua/plenary.nvim", },
    keys = function()
      local result = {}
      for i, key in ipairs(keys) do
        result[i] = { key[1], nil, desc = key[3] }
      end
      return result
    end,
    config = function()
      local builtin = require('telescope.builtin')
      for _, key in ipairs(keys) do
        vim.keymap.set("n", key[1], function() key[2](builtin) end, { desc = key[3] })
      end
    end,
  },

  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    keys = {
      { "<leader>sg", nil, desc = "with [G]rep (root dir)" },
      { "<leader>sG", nil, desc = "with [G]rep (cwd)" },
    },
    config = function()
      local telescope = require("telescope")
      local lga_actions = require("telescope-live-grep-args.actions")

      telescope.setup {
        defaults = {
          file_sorter = require("mini.fuzzy").get_telescope_sorter,
          generic_sorter = require("mini.fuzzy").get_telescope_sorter,
        },
        extensions = {
          live_grep_args = {
            mappings = {
              i = {
                ["<C-k>"] = lga_actions.quote_prompt(),
                ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
              },
            },
          }
        }
      }

      local live_grep_args = telescope.extensions.live_grep_args.live_grep_args
      vim.keymap.set("n", "<leader>sg", function() live_grep_args({ cwd = Util.get_root() }) end, { desc = "with [G]rep (root dir)" })
      vim.keymap.set("n", "<leader>sG", function() live_grep_args() end, { desc = "with [G]rep (cwd)" })
    end,
  },
}
