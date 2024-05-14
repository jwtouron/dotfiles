vim.opt.autowrite = true
vim.opt.completeopt = { 'menu', 'menuone', 'noinsert', 'noselect', }
vim.opt.expandtab = true
vim.opt.ignorecase = true
vim.opt.jumpoptions = { "stack" }
vim.opt.list = true
vim.opt.listchars =  { tab = "» " }
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 4
vim.opt.shiftwidth = 0
vim.opt.sidescrolloff = 8
vim.opt.smartcase = true
vim.opt.softtabstop = 0
vim.opt.tabstop = 4
vim.opt.termguicolors = true
vim.opt.wrap = false

if vim.fn.executable("rg") then
  vim.opt.grepprg = "rg --smart-case --vimgrep"
end

vim.g.netrw_winsize = 25
