return {
  "folke/which-key.nvim",
  enabled = false,
  config = function()
    local wk = require("which-key")
    wk.setup()
    wk.register({
      ["<leader>f"] = { name = "Telescope" },
      ["<leader>u"] = { name = "ui" },
    })
  end,
}
