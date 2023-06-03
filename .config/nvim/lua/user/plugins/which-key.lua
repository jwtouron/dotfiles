return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  config = function()
    local wk = require("which-key")
    wk.setup()
    wk.register({
      ["<tab>"] = { name = "Tabs" },
      b = { name = "[B]uffer" },
      c = { name = "[C]ode" },
      f = { name = "[F]ile" },
      g = { name = "[G]it" },
      s = { name = "[S]earch (telescope)" },
      u = { name = "[U]I" },
      w = { name = "[W]indow" },
      x = { name = "Trouble" },
    }, { prefix = "<leader>" })
  end,
}
