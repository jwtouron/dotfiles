-- Build custom help tags
local docdir = vim.fn.stdpath("config") .. "/doc"
local tagfile = docdir .. "/tags"

if vim.fn.filereadable(tagfile) == 0 then
  vim.schedule(function() vim.cmd("helptags " .. vim.fn.fnameescape(docdir)) end)
end
