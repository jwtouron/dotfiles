vim.opt.expandtab = true
vim.opt.shiftwidth = 4

vim.opt.breakindent = true
vim.opt.completeopt = { "menuone", "noinsert", "noselect" }
vim.opt.formatoptions = 'qjl1'
vim.opt.gdefault = true
vim.opt.ignorecase = true
vim.opt.infercase = true
vim.opt.linebreak = true
vim.opt.list = true
vim.opt.listchars = { tab = "> ", extends = '…', precedes = '…', nbsp = '␣' }
vim.opt.mouse = 'a'
vim.opt.number = true
vim.opt.pumblend  = 10
vim.opt.pumheight = 10
vim.opt.relativenumber = true
vim.opt.scrolloff = 5
vim.opt.showmode = false
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.swapfile = false
vim.opt.termguicolors = true
vim.opt.updatetime = 250
vim.opt.virtualedit = 'block'
vim.opt.winblend  = 10
vim.opt.wrap = false

if vim.fn.executable("rg") then
  vim.opt.grepprg = 'rg --vimgrep --smart-case --hidden'
  vim.opt.grepformat = '%f:%l:%c:%m'
end
