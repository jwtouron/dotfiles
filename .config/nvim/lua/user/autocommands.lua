local MyAutocommands = vim.api.nvim_create_augroup("MyAutocommands", { clear = true })

vim.api.nvim_create_autocmd("TextYankPost", {
  group = MyAutocommands,
  callback = function() vim.highlight.on_yank() end,
})

-- FileType autocommands

local function filetype(ft, callback)
  vim.api.nvim_create_autocmd("FileType", {
    group = MyAutocommands,
    pattern = ft,
    callback = callback,
  })
end

filetype("fugitive", function()
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
  vim.opt_local.cursorline = true
end)

filetype("go", function()
  vim.opt_local.expandtab = false
  vim.opt_local.shiftwidth = 8
  vim.opt_local.tabstop = 8
end)

filetype("help", function()
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
end)

filetype("lua", function()
  vim.opt_local.tabstop = 2
end)

filetype("qf", function()
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
end)
