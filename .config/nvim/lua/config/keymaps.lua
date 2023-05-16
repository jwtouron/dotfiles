-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local Util = require("lazyvim.util")

-- Execute macro saved at 'q'
vim.keymap.set("n", "Q", "@q")

-- Easy editing of words (cg* on a word, edit, then '.' or 'n')
vim.keymap.set("n", "cg*", "*Ncgn")

-- Don't overwrite paste register when pasting in visual mode
vim.keymap.set("x", "p", [["_dP]])

-- Don't move cursor when joining lines (uses 'z' mark)
vim.keymap.set("n", "J", "mzJ`z")

-- Ranger integration
vim.keymap.set("n", "<leader>rr", function() Util.float_term({ "ranger" }, { cwd = Util.get_root(), esc_esc = false }) end, { desc = "Ranger (root dir)" })
vim.keymap.set("n", "<leader>rR", function() Util.float_term({ "ranger" }, {esc_esc = false}) end, { desc = "RangerR(cwd)" })

-- Toggle cursorline
vim.keymap.set("n", "<leader>uL", function() Util.toggle("cursorline") end, { desc = "Toggle Cursorline" })
