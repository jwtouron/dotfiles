vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move cursor to Nth window left of current one." })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move cursor to Nth window below current one." })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move cursor to Nth window above current one." })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move cursor to Nth window right of current one." })

vim.keymap.set("n", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true })
vim.keymap.set("x", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true })
vim.keymap.set("n", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true })
vim.keymap.set("x", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true })

vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

vim.keymap.set("n", "cg*", "*Ncgn", { desc = "Change word under cursor, '.' to continue." })

-- https://vim.fandom.com/wiki/Search_for_visually_selected_text
vim.keymap.set("x", "*", [[y/\V<C-R>=escape(@",'/\')<CR><CR>]])
vim.keymap.set("x", "#", [[y?\V<C-R>=escape(@",'/\')<CR><CR>]])

-- Don't overwrite paste register when pasting in visual mode
vim.keymap.set("x", "p", [["_dp]], { desc = "Paste in visual mode without overwriting paste register." })
vim.keymap.set("x", "P", [["_dP]], { desc = "Paste in visual mode without overwriting paste register." })

-- Resize window using <ctrl> arrow keys
vim.keymap.set("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- Don't move cursor when joining lines (uses 'z' mark)
vim.keymap.set("n", "J", "mzJ`z", { silent = true, desc = "Join lines" })
