local MyAutocommands = vim.api.nvim_create_augroup("MyAutocommands", { clear = true })

local filetype_autocmds = {
  { "go", { command = "setlocal tabstop=8 shiftwidth=8 noexpandtab" } },
  { "help", { command = "nnoremap <buffer> <silent> q :q<cr>" } },
  { "lua", { command = "setlocal tabstop=2" } },
  { "qf", { command = "nnoremap <buffer> <silent> q :q<cr>" } },
}

for _, autocmd in ipairs(filetype_autocmds) do
  local opts = autocmd[2] or {}
  opts.pattern = autocmd[1]
  opts.group = MyAutocommands,
  vim.api.nvim_create_autocmd("FileType", opts)
end

vim.api.nvim_create_autocmd("TextYankPost", {
  group = MyAutocommands,
  callback = function() vim.highlight.on_yank() end,
})
