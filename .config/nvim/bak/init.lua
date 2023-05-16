vim.g.mapleader = " "

require("user.autocommands")
require("user.commands")
require("user.mappings")
require("user.options")
require("user.plugins")

pcall(require, "user.custom")
