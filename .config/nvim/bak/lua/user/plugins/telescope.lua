local keys = {
  b = "buffers",
  c = "colorscheme",
  f = "find_files",
  g = "live_grep",
  h = "help_tags",
  k = "keymaps",
  l = "loclist",
  m = "man_pages",
  q = "quickfix",
  r = "registers",
  R = "resume",
}

local builtin = nil

return {
  {
    'nvim-telescope/telescope.nvim', tag = '0.1.1',
    dependencies = { 'nvim-lua/plenary.nvim' },
    keys = function()
      local f = function(name)
        return function()
          builtin[name]()
        end
      end
      local result = {
        { "<leader><leader>", f("find_files"), desc = "Telescope: find_files" },
        { "<leader>,", f("buffers"), desc = "Telescope: buffers" },
        { "<leader>f/", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Telescope: current_buffer_fuzzy_find" },
      }
      for k, v in pairs(keys) do
        table.insert(result, { "<leader>f"..k, f(v), desc = "Telescope: "..v })
      end
      return result
    end,
    config = function()
      builtin = require("telescope.builtin")
    end,
  }
}
