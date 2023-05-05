-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "cg*", "*Ncgn")

-- Don't overwrite paste register when pasting in visual mode
vim.keymap.set("x", "p", [["_dP]])

-- Don't move cursor when joining lines (uses 'z' mark)
vim.keymap.set("n", "J", "mzJ`z")
