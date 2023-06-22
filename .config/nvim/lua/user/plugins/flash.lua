return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {},
  keys = {
    {
      "s",
      mode = { "n", "x", "o" },
      function()
        require("flash").jump()
      end,
    },
    {
      "S",
      mode = { "n", "o", "x" },
      function()
        require("flash").treesitter()
      end,
    },
  },
}
