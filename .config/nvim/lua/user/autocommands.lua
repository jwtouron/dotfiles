vim.api.nvim_create_autocmd("TextYankPost", {
  group = MyAugroup,
  callback = function() vim.highlight.on_yank() end,
})

-- FileType autocommands

function CreateFileTypeAutocmd(ft, callback)
  vim.api.nvim_create_autocmd("FileType", {
    group = MyAugroup,
    pattern = ft,
    callback = callback,
  })
end

CreateFileTypeAutocmd("qf", function()
  vim.opt_local.buflisted = false
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
  vim.keymap.set("n", "Q", "q", { buffer = true, })
  vim.keymap.set("n", "<C-j>", "<cmd>:cnewer<cr>", { buffer = true, })
  vim.keymap.set("n", "<C-k>", "<cmd>:colder<cr>", { buffer = true, })
end)

CreateFileTypeAutocmd("go", function()
  vim.opt_local.expandtab = false
  vim.opt_local.shiftwidth = 8
  vim.opt_local.tabstop = 8
end)

CreateFileTypeAutocmd("help", function()
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
end)

CreateFileTypeAutocmd("lua", function()
  vim.opt_local.tabstop = 2
end)
