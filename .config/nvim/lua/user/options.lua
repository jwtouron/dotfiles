vim.opt.autowrite = true
vim.opt.completeopt = { "menu", "menuone", "noinsert", "noselect" }
vim.opt.expandtab = true
vim.opt.gdefault = true
vim.opt.ignorecase = true
vim.opt.list = true
vim.opt.mouse = 'a'
vim.opt.mousemoveevent = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 4
vim.opt.shiftwidth = 0  -- When zero the 'ts' value will be used.
vim.opt.sidescrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.smartcase = true
vim.opt.swapfile = false
vim.opt.tabstop = 4
vim.opt.termguicolors = true
vim.opt.timeoutlen = 500
vim.opt.wrap = false

if vim.fn.executable("rg") then
  vim.opt.grepprg = "rg --vimgrep --smart-case"
  vim.opt.grepformat = '%f:%l:%c:%m'
end
