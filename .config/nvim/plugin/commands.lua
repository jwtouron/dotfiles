vim.api.nvim_create_user_command("PackUpdate", function(arg)
  vim.pack.update(nil, { force = arg.bang })
end,
{ bang = true })
