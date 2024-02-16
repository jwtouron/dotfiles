vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
MyAugroup = vim.api.nvim_create_augroup("MyAugroup", { clear = true })

require("user.autocommands")
require("user.commands")
require("user.diagnostic")
require("user.lazy")
require("user.mappings")
require("user.options")

pcall(require, "user.custom")
