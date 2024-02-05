return {
  "folke/which-key.nvim",
  enabled = false,
  event = "VeryLazy",
  config = function()
    local wk = require("which-key")
    wk.setup()
    wk.register({
      ["<leader>"] = {
        ["<tab>"] = { name = "Tabs" },
        b = { name = "[B]uffer" },
        e = { name = "[E]Executor" },
        f = { name = "[F]ile" },
        t = { name = "[T]elescope" },
        w = { name = "[W]indow" },
        x = { name = "Trouble" },
      }
    })
  end,
}
