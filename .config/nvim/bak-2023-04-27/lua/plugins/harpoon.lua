local function config()
  local mark = require("harpoon.mark")
  local ui = require("harpoon.ui")
  vim.keymap.set("n", "<leader>ha", function() mark.add_file() end)
  vim.keymap.set("n", "<leader>hm", function() ui.toggle_quick_menu() end)
  vim.keymap.set("n", "<leader>hn", function() ui.nav_next() end)
  vim.keymap.set("n", "<leader>hp", function() ui.nav_prev() end)

  vim.keymap.set("n", "<leader>h1", function() ui.nav_file(1) end)
  vim.keymap.set("n", "<leader>h2", function() ui.nav_file(2) end)
  vim.keymap.set("n", "<leader>h3", function() ui.nav_file(3) end)
  vim.keymap.set("n", "<leader>h4", function() ui.nav_file(4) end)
  vim.keymap.set("n", "<leader>h5", function() ui.nav_file(5) end)
  vim.keymap.set("n", "<leader>h6", function() ui.nav_file(6) end)
  vim.keymap.set("n", "<A-1>",      function() ui.nav_file(1) end)
  vim.keymap.set("n", "<A-2>",      function() ui.nav_file(2) end)
  vim.keymap.set("n", "<A-3>",      function() ui.nav_file(3) end)
  vim.keymap.set("n", "<A-4>",      function() ui.nav_file(4) end)
  vim.keymap.set("n", "<A-5>",      function() ui.nav_file(5) end)
  vim.keymap.set("n", "<A-6>",      function() ui.nav_file(6) end)

  require("telescope").load_extension("harpoon")
  vim.keymap.set("n", "<leader>fh", function() vim.cmd("Telescope harpoon marks") end)
end

return {
  'ThePrimeagen/harpoon',
  dependencies = 'nvim-lua/plenary.nvim',
  config = config,
}
