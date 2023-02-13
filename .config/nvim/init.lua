vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require "plugins"
require "options"
require "keymaps"
require "colorscheme"

vim.cmd [[
command! BD b#|bd#
command! Bd b#|bd#
command! BW b#|bw#
command! Bw b#|bw#
]]
