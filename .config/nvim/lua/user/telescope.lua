local status_ok, telescope = pcall(require, 'telescope.builtin')
if not status_ok then
    return
end

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap("n", "<leader>f", "<cmd>Telescope find_files<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>Telescope live_grep<cr>", opts)
vim.api.nvim_set_keymap("n", "<leader>b", "<cmd>Telescope buffers<cr>", opts)
