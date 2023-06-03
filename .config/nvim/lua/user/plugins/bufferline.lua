return {
  "akinsho/bufferline.nvim",
  version = "*",
  dependencies = "nvim-tree/nvim-web-devicons",
  event = "VeryLazy",
  opts = {
    options = {
      close_command = function(n) require("mini.bufremove").delete(n, false) end,
      right_mouse_command = function(n) require("mini.bufremove").delete(n, false) end,
      offsets = {
        {
          filetype = "neo-tree",
          text = "Neo-tree",
          text_align = "center",
          highlight = "Directory",
          separator = true,
        }
      },
      always_show_bufferline = true,
      indicator = {
        style = "underline",
      },
      diagnostics = "nvim_lsp",
      hover = {
        enabled = true,
        delay = 200,
        reveal = { "close" }
      },
    }
  }
}
