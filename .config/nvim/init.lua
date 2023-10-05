pcall(vim.loader.enable)  -- Improve startup time

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
MyAugroup = vim.api.nvim_create_augroup("MyAugroup", { clear = true })

require("user.functions")
require("user.autocommands")
require("user.commands")
require("user.mappings")
require("user.options")
require("user.lazy")

pcall(require, "user.custom")
