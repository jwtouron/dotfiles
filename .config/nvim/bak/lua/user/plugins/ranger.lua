local ranger_nvim = nil

return {
  "kelly-lin/ranger.nvim",
  keys = {
    {
      "<leader>r",
      function()
        ranger_nvim.open(true)
      end,
      desc = "ranger"
    },
  },
  config = function()
    ranger_nvim = require("ranger-nvim")
    ranger_nvim.setup({ replace_netrw = true })
  end,
}
