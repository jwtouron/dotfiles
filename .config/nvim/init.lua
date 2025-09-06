vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("user.autocommands")
require("user.commands")
require("user.diagnostic")
require("user.lazy")
require("user.mappings")
require("user.options")
-- require("user.completion")

-- exec.nvim
-- cfilter
-- auto-update qflist
-- mini.hues
-- ft filse

pcall(require, "user.custom")
