local Util = require("user.util")

return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v2.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim",
  },
  cmd = "Neotree",
  keys = {
    { "<leader>ft", nil, desc = "Neotree (root dir)" },
    { "<leader>fT", nil, desc = "Neotree (cwd)" },
  },
  opts = {
    buffers = {
      bind_to_cwd = false,
    },
    enable_git_status = false,
    filesystem = {
      bind_to_cwd = false,
    },
    popup_border_style = "rounded",
    window = {
      width = 20,
    },
  },
  init = function()
    vim.g.neo_tree_remove_legacy_commands = 1
  end,
  config = function(_, opts)
    require("neo-tree").setup(opts)

    local N = function(dir)
      return "<cmd>Neotree toggle dir=" .. dir .. "<cr>"
    end

    vim.keymap.set("n", "<leader>ft", N(Util.get_root()), { desc = "Neotree (root dir)" })
    vim.keymap.set("n", "<leader>fT", N(vim.loop.cwd()), { desc = "Neotree (cwd)" })
  end
}
