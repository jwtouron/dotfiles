vim.cmd.packadd('cfilter')
vim.cmd.packadd('nohlsearch')
vim.cmd.packadd('nvim.undotree')

require('vim._core.ui2').enable {}

vim.g.better_escape_shortcut = { 'jk', 'kj' }
vim.pack.add({ "https://github.com/nvim-zh/better-escape.vim" }, { confirm = false, })

vim.pack.add(
  {
    "https://github.com/tpope/vim-repeat",
    "https://github.com/tpope/vim-rsi",
    "https://github.com/tpope/vim-sleuth",
  }, { confirm = false }
)

-- vim.pack.add(
--   {
--     "http://github.com/jeetsukumaran/vim-indentwise",
--     "http://github.com/michaeljsmith/vim-indent-object",
--   }, { confirm = false }
-- )

-- vim.keymap.set({ "n", "x", "o" }, "<c-h>", "<Plug>(IndentWisePreviousLesserIndent)")
-- vim.keymap.set({ "n", "x", "o" }, "<c-j>", "<Plug>(IndentWiseNextEqualIndent)")
-- vim.keymap.set({ "n", "x", "o" }, "<c-k>", "<Plug>(IndentWisePreviousEqualIndent)")
-- vim.keymap.set({ "n", "x", "o" }, "<c-l>", "<Plug>(IndentWiseNextGreaterIndent)")
