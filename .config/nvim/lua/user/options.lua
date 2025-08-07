local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.opt.autowrite = true
vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noinsert', 'noselect', 'popup', }
vim.opt.cursorline = true
vim.opt.cursorlineopt = 'number'
vim.opt.expandtab = true
vim.opt.exrc = true
vim.opt.ignorecase = true
-- vim.opt.inccommand = 'split'
vim.opt.jumpoptions = { "stack" }
vim.opt.list = true
vim.opt.listchars =  { tab = "» " }
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 4
vim.opt.shiftwidth = 0  -- When 0, use value of tabstop
vim.opt.sidescrolloff = 8
vim.opt.smartcase = true
-- vim.opt.softtabstop = 0  -- Default is 0, 0 means off
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabstop = 4
-- vim.opt.termguicolors = true  -- Neovim will automatically detect and enable
vim.opt.updatetime = 1000  -- Needed for CursorHold
vim.opt.wildmode = { 'noselect:lastused', 'full' }
vim.opt.wildoptions:append('fuzzy')
vim.opt.winborder = 'rounded'
vim.opt.wrap = false
vim.opt.wrapscan = false

vim.g.netrw_winsize = 25

vim.api.nvim_create_autocmd("CmdlineEnter", {
  group = augroup,
  callback = function()
    local dirs = ''
    for _, path in ipairs(vim.opt.path:get()) do
      if path == "." then
      elseif path == "" then
        dirs = dirs .. " ."
      else
        dirs = dirs .. " " .. path
      end
    end

    if vim.fn.executable('rg') == 1 then
      vim.o.grepprg = vim.o.grepprg .. " $*" .. dirs
      vim.opt.grepformat = '%f:%l:%c:%m'
    else
      vim.o.grepprg = string.gsub(vim.o.grepprg, '[$][*].*', '-r $*' .. dirs)
      print(vim.o.grepprg)
      vim.opt.grepformat = '%f:%l:%m'
    end
  end,
  once = true,
})
