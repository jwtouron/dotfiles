vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require "plugins"
require "options"
require "keymaps"
require "commands"
require "colorscheme"

pcall(require, 'custom')
