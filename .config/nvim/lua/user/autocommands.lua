local augroup = vim.api.nvim_create_augroup("db35933f-c123-4261-9041-f2895fcf9fb5", { clear = true })

-- Jump to last location when opening a buffer.
vim.api.nvim_create_autocmd({ "BufReadPost" }, {
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

create_filetype_autocmd("go", "setlocal tabstop=8 noexpandtab")

create_filetype_autocmd("help", "nnoremap <buffer> q <cmd>q<cr>")

create_filetype_autocmd("lua", "setlocal tabstop=2")

create_filetype_autocmd({"markdown", "text"}, "setlocal wrap linebreak breakindent")
