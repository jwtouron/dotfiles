vim.pack.add(
  {
    { src = "https://github.com/nvim-lua/plenary.nvim" },
    { src = "https://github.com/ThePrimeagen/harpoon", version = "harpoon2", },
  }, { confirm = false }
)

local harpoon = require("harpoon")

harpoon:setup { settings = { save_on_toggle = true } }

vim.keymap.set("n", "<A-a>", function() harpoon:list():add() end)
vim.keymap.set("n", "<A-m>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

vim.keymap.set("n", "<A-1>", function() harpoon:list():select(1) end)
vim.keymap.set("n", "<A-2>", function() harpoon:list():select(2) end)
vim.keymap.set("n", "<A-3>", function() harpoon:list():select(3) end)
vim.keymap.set("n", "<A-4>", function() harpoon:list():select(4) end)
vim.keymap.set("n", "<A-5>", function() harpoon:list():select(5) end)
vim.keymap.set("n", "<A-6>", function() harpoon:list():select(6) end)

vim.keymap.set("n", "<A-h>", function() harpoon:list():prev({ ui_nav_wrap = true }) end)
vim.keymap.set("n", "<A-l>", function() harpoon:list():next({ ui_nav_wrap = true }) end)
