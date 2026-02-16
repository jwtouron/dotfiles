local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.opt.autowrite = true
vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noinsert', 'noselect', 'popup', }
vim.opt.cursorline = true
vim.opt.cursorlineopt = 'number'
vim.opt.expandtab = true
vim.opt.exrc = true
vim.opt.ignorecase = true
-- vim.opt.inccommand = 'split'
vim.opt.jumpoptions = { "stack", "clean", }
vim.opt.list = true
vim.opt.listchars =  { tab = "» " }
-- vim.opt.number = true
vim.opt.pumblend = 10
-- vim.opt.relativenumber = true
vim.opt.scrolloff = 4
vim.opt.shiftround = true
vim.opt.shiftwidth = 0  -- When 0, use value of tabstop
vim.opt.sidescrolloff = 8
vim.opt.smartcase = true
-- vim.opt.softtabstop = 0  -- Default is 0, 0 means off
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.suffixes:append { ".class", ".pyc", ".pyo" }
vim.opt.swapfile = false
vim.opt.tabstop = 4
-- vim.opt.termguicolors = true  -- Neovim will automatically detect and enable
vim.opt.updatetime = 1000  -- Needed for CursorHold
vim.opt.wildignore:append {
  '**/.git/**', '**/.DS_Store', '**/.cache/**', '**/*.pyc', '**/*.o', '**/*.obj',
  '**/node_modules/**', '**/dist/**', '**/build/**', '**/coverage/**', '**/__pycache__/**',
}
vim.opt.wildignorecase = true
vim.opt.wildmode = { 'noselect:lastused:full', 'full' }
vim.opt.wildoptions:append('fuzzy')
vim.opt.winborder = 'rounded'
vim.opt.wrap = false

vim.g.netrw_winsize = 25

-- function MyFindFunc(cmdarg)
--   local cmd = ''
--
--   if vim.fn.executable('fd') == 1 then
--     cmd = "fd --type file --unrestricted --ignore-case --exclude '.git'"
--     local paths = {}
--     for _, path in ipairs(vim.opt.path:get()) do
--       if path ~= "." then
--         if path == "" then
--           paths["."] = true
--         else
--           path = path:gsub("[*][*]", "")
--           if vim.fn.isdirectory(vim.fn.expand(path)) ~= 0 then
--             paths[path] = true
--           end
--         end
--       end
--     end
--     for path in pairs(paths) do
--       cmd = cmd .. ' --search-path ' .. path
--     end
--   elseif vim.fn.executable('rg') == 1 then
--   else
--   end
--
--   cmd = cmd .. ' | fzf -f ' .. vim.fn.shellescape(cmdarg)
--
--   return vim.fn.systemlist(cmd)
-- end
--
-- vim.opt.findfunc = 'v:lua.MyFindFunc'
