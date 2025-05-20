local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

-- Jump to last location when opening a buffer.
vim.api.nvim_create_autocmd({ "BufReadPost" }, {
  group = augroup,
  pattern = "*",
  callback = function()
    vim.api.nvim_exec2('silent! normal! g`"zvzz', {})
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = augroup,
  pattern = "*",
  command = "lua vim.highlight.on_yank()",
})

-- FileType autocommands

local function create_filetype_autocmd(pattern, command)
  local opts = {
    group = augroup,
    pattern = pattern,
  }
  if type(command) == 'string' then
    opts.command = command
  else
    opts.callback = command
  end
  vim.api.nvim_create_autocmd("FileType", opts)
end

local ft_commands = {
  c3        = "setlocal smartindent errorformat=(%f:%l:%c)\\ %m",
  go        = "setlocal tabstop=8 noexpandtab",
  -- help      = "nnoremap <buffer> q <cmd>q<cr>",
  lua       = "setlocal tabstop=2",
  markdown  = "setlocal wrap linebreak breakindent tabstop=2 conceallevel=2 concealcursor= nofoldenable",
  text      = "setlocal wrap linebreak breakindent",
  vim       = "setlocal tabstop=2",
  zig       = "let g:zig_fmt_autosave = 0",
}
for ft, cmd in pairs(ft_commands) do
  create_filetype_autocmd(ft, cmd)
end
