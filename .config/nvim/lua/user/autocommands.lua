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

CreateFileTypeAutocmd("qf", function()
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
end)
