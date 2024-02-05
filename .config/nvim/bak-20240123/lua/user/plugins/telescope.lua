local Util = require("user.util")

local keys = {
  { "<leader><leader>", function(builtin) builtin.find_files({ cwd = Util.get_root() }) end, "Search Files (root dir)" },
  { "<leader>,", function(builtin) builtin.buffers({ sort_lastused = true }) end, "Search Buffers" },
  { "<leader>/", function(builtin) builtin.current_buffer_fuzzy_find() end, "Current Buffer Fuzzy Find" },

  { "<leader>tb", function(builtin) builtin.buffers({ sort_lastused = true }) end, "[B]uffers" },
  { "<leader>tc", function(builtin) builtin.commands() end, "[C]ommands" },
  { "<leader>tC", function(builtin) builtin.command_history() end, "[C]ommand History" },
  { "<leader>td", function(builtin) builtin.diagnostics({ bufnr = 0}) end, "[D]iagnostics (current buffer)" },
  { "<leader>tD", function(builtin) builtin.diagnostics({ bufnr = nil}) end, "[D]iagnostics (all buffers)" },
  { "<leader>tf", function(builtin) builtin.find_files({ cwd = Util.get_root() }) end, "[F]iles (root dir)" },
  { "<leader>tF", function(builtin) builtin.find_files() end, "[F]iles (cwd)" },
  { "<leader>th", function(builtin) builtin.help_tags() end, "[H]elp" },
  { "<leader>tk", function(builtin) builtin.keymaps() end, "[K]eymaps" },
  { "<leader>tl", function(builtin) builtin.loclist() end, "[L]oclist" },
  { "<leader>tm", function(builtin) builtin.marks() end, "[M]arks" },
  { "<leader>tM", function(builtin) builtin.man_pages() end, "[M]an Pages" },
  { "<leader>to", function(builtin) builtin.oldfiles() end, "[O]ldfiles" },
  { "<leader>tq", function(builtin) builtin.quickfix() end, "[Q]uickfix" },
  { "<leader>tQ", function(builtin) builtin.quickfixhistory() end, "[Q]uickfix History" },
  { "<leader>tr", function(builtin) builtin.registers() end, "[R]egisters" },
  { "<leader>tR", function(builtin) builtin.resume() end, "[R]esume" },
  { "<leader>ts", function(builtin) builtin.spell_suggest() end, "[S]pell Suggestions" },
  { "<leader>tS", function(builtin) builtin.colorscheme({ enable_preview = true }) end, "Color[S]chemes" },

  -- LSP
  { "<leader>tLc", function(builtin) builtin.lsp_incoming_calls() end, "[L]SP Incoming [C]alls" },
  { "<leader>tLC", function(builtin) builtin.lsp_outgoing_calls() end, "[L]SP Outgoing [C]alls" },
  { "<leader>tLd", function(builtin) builtin.lsp_definitions() end, "[L]SP [D]efinitions" },
  { "<leader>tLi", function(builtin) builtin.lsp_implementations() end, "[L]SP [I]mplementations" },
  { "<leader>tLr", function(builtin) builtin.lsp_references() end, "[L]SP [R]eferences" },
  { "<leader>tLs", function(builtin) builtin.lsp_document_symbols() end, "[L]SP Document [S]ymbols" },
  { "<leader>tLS", function(builtin) builtin.lsp_workspace_symbols() end, "[L]SP Workspace [S]ymbols" },
  { "<leader>tLt", function(builtin) builtin.lsp_type_definitions() end, "[L]SP [T]ype Definitions" },
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
      { "<leader>tg", nil, desc = "[T]elescope [G]rep (root dir)" },
      { "<leader>tG", nil, desc = "[T]elescope [G]rep (cwd)" },
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
      vim.keymap.set("n", "<leader>tg", function() live_grep_args({ cwd = Util.get_root() }) end, { desc = "with [T]elescope (root dir)" })
      vim.keymap.set("n", "<leader>tG", function() live_grep_args() end, { desc = "with [T]elescope (cwd)" })
    end,
  },
}
