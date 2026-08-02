local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

local autocmds = {

  {
    filetype = "css",
    command = "setlocal tabstop=2"
  },

  {
    filetype = "go",
    command = "setlocal noexpandtab"
  },

  {
    filetype = "gomod",
    command = "setlocal noexpandtab"
  },

  {
    filetype = "html",
    command = "setlocal tabstop=2"
  },

  {
    filetype = "js",
    command = "setlocal tabstop=2"
  },

  {
    filetype = "lua",
    command = "setlocal tabstop=2"
  },

  {
    filetype = "markdown",
    command = "setlocal tabstop=2"
  },

  {
    filetype = "netrw",
    callback = function()
      vim.keymap.set('n', 'g?', "<cmd>help netrw-browse-maps<cr>", { buffer = 0 })
    end
  },

  {
    filetype = "qf",
    command = "setlocal cursorlineopt=both"
  },

}

for _, autocmd in ipairs(autocmds) do
  local opts = { pattern = autocmd.filetype }
  opts.command = autocmd.command
  opts.callback = autocmd.callback
  vim.api.nvim_create_autocmd("FileType", opts)
end
