vim.cmd.packadd('cfilter')
vim.cmd.packadd('nohlsearch')
vim.cmd.packadd('nvim.undotree')

vim.g.better_escape_shortcut = { 'jk', 'kj' }
vim.pack.add({ "https://github.com/nvim-zh/better-escape.vim" }, { confirm = false, })

vim.pack.add(
  {
    "https://github.com/tpope/vim-repeat",
    "https://github.com/tpope/vim-rsi",
    "https://github.com/tpope/vim-sleuth",
  }, { confirm = false }
)
