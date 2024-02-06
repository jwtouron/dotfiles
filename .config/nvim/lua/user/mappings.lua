vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move cursor to Nth window left of current one." })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move cursor to Nth window below current one." })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move cursor to Nth window above current one." })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move cursor to Nth window right of current one." })

vim.keymap.set("n", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true })
vim.keymap.set("x", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true })
vim.keymap.set("n", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true })
vim.keymap.set("x", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true })
