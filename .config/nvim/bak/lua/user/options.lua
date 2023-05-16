vim.opt.completeopt = { "menuone", "noinsert", "noselect" }
vim.opt.expandtab = true
vim.opt.gdefault = true
vim.opt.ignorecase = true
vim.opt.list = true
vim.opt.mouse = 'a'
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 4
vim.opt.shiftwidth = 0  -- Use the value of 'tabstop'
vim.opt.sidescrolloff = 8
vim.opt.signcolumn = 'yes'
vim.opt.smartcase = true
vim.opt.tabstop = 4
vim.opt.timeoutlen = 500
vim.opt.updatetime = 500
-- vim.opt.winbar = "%=%m %f"
vim.opt.wrap = false

vim.g.netrw_liststyle = 3

if vim.fn.executable("rg") then
  vim.opt.grepprg = "rg --vimgrep --smart-case --hidden"
  vim.opt.grepformat = "%f:%l:%c:%m"
end
