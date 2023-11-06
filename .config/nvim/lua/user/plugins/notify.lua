return {
  'rcarriga/nvim-notify',
  dependencies = "nvim-telescope/telescope.nvim",
  event = "VeryLazy",
  config = function()
    local notify = require("notify")
    notify.setup()

    vim.notify = notify

    require("telescope").load_extension("notify")
    vim.keymap.set("n", "<leader>tn", "<cmd>Telescope notify<cr>", { desc = "[T]elescope [N]otify" })
  end,
}
