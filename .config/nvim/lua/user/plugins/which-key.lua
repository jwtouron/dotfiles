return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  config = function()
    local wk = require("which-key")
    wk.setup()
    wk.register({
      ["<leader>"] = {
        ["<tab>"] = { name = "Tabs" },
        b = { name = "[B]uffer" },
        f = { name = "[F]ile" },
        g = { name = "[G]it" },
        s = { name = "[S]earch (telescope)" },
        u = { name = "[U]I" },
        w = { name = "[W]indow" },
        x = { name = "Trouble" },
      }
    })
  end,
}
