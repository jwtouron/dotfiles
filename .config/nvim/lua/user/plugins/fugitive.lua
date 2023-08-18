return {
  "tpope/vim-fugitive",
  cmd = { "G", "Git", "GitGrep" },
  config = function()
    CreateFileTypeAutocmd("fugitive", function()
      vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
      vim.opt_local.cursorline = true
    end)
    CreateFileTypeAutocmd("git", function()
      vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
    end)
  end,
}
