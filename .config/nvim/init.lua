vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("user.autocommands")
require("user.commands")
require("user.diagnostic")
require("user.lazy")
require("user.mappings")
require("user.options")
require("user.misc")
-- require("user.completion")

pcall(require, "user.custom")
