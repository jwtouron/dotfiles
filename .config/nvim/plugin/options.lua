vim.opt.autowrite = true
vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noinsert', 'noselect', 'popup', }
vim.opt.cursorline = true
vim.opt.cursorlineopt = 'number'
vim.opt.expandtab = true
vim.opt.exrc = true
vim.opt.ignorecase = true
vim.opt.jumpoptions = { 'stack', 'clean', }
vim.opt.list = true
vim.opt.listchars =  { tab = "» " }
vim.opt.mouse = 'a'
vim.opt.number = true
vim.opt.pumblend = 15
vim.opt.pumborder = 'rounded'
vim.opt.relativenumber = true
vim.opt.scrolloff = 4
vim.opt.shiftround = true
vim.opt.shiftwidth = 0  -- Use value of 'tabstop'
vim.opt.shortmess:append 'I'
vim.opt.sidescrolloff = 8
vim.opt.smartcase = true
vim.opt.smoothscroll = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabstop = 4
vim.opt.updatetime = 1000  -- Affects CursorHold
vim.opt.wildignorecase = true
vim.opt.wildmode = { 'noselect:lastused:full', 'full' }
vim.opt.wildoptions:append('fuzzy')
vim.opt.winborder = 'rounded'
vim.opt.wrap = false

if vim.env.WAYLAND_DISPLAY
  and vim.env.WAYLAND_DISPLAY ~= ''
  and vim.fn.executable('wl-copy') == 1
then
  vim.g.clipboard = {
    name = 'wl-copy',
    copy = {
      ['+'] = 'wl-copy --foreground',
      ['*'] = 'wl-copy --foreground',
    },
    paste = {
      ['+'] = 'wl-paste --no-newline',
      ['*'] = 'wl-paste --no-newline',
    },
    cache_enabled = 1,
  }
end
