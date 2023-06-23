pcall(vim.loader.enable)  -- Improve startup time

vim.g.mapleader = " "

require("user.autocommands")
require("user.commands")
require("user.mappings")
require("user.options")
require("user.plugins-bootstrap")

pcall(require, "user.custom")
