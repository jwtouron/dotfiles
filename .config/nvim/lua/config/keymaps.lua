-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local Util = require("lazyvim.util")
local Terminal = require("toggleterm.terminal").Terminal

-- Execute macro saved at 'q'
vim.keymap.set("n", "Q", "@q")

-- Easy editing of words (cg* on a word, edit, then '.' or 'n')
vim.keymap.set("n", "cg*", "*Ncgn")

-- Don't overwrite paste register when pasting in visual mode
vim.keymap.set("x", "p", [["_dP]])

-- Don't move cursor when joining lines (uses 'z' mark)
vim.keymap.set("n", "J", "mzJ`z")

local function toggleterm(cmd, opts)
  local args = {
    cmd = cmd,
    hidden = true,
    direction = "float",
    highlights = {
      NormalFloat = { link = 'Normal' },
      FloatBorder = { link = 'Normal' },
    },
    shade_terminals = false,
  }
  for k, v in pairs(opts or {}) do
    args[k] = v
  end
  return Terminal:new(args)
end

local function xplr(opts)
  local fname = vim.fn.tempname()
  opts = opts or {}
  opts.on_exit = function()
    if vim.fn.filereadable(fname) ~= 0 then
      for _, line in ipairs(vim.fn.readfile(fname)) do
        if vim.fn.filereadable(line) ~= 0 then
          vim.cmd.edit(line)
        end
      end
      vim.fn.delete(fname)
    end
  end
  toggleterm("xplr > " .. fname, opts):open()
end

-- xplr

vim.keymap.set("n", "<leader>fm", function()
  xplr({ dir = Util.get_root() })
end, { desc = "lf (root dir)" })
vim.keymap.set("n", "<leader>fM", function()
  xplr()
end, { desc = "lf (cwd)" })

-- lazygit

vim.keymap.set("n", "<leader>gg", function()
  toggleterm("lazygit", { dir = Util.get_root() }):open()
end, { desc = "Lazygit (root dir)" })

vim.keymap.set("n", "<leader>gG", function()
  toggleterm("lazygit"):open()
end, { desc = "Lazygit (cwd)" })

-- Toggle cursorline
vim.keymap.set("n", "<leader>uL", function()
  Util.toggle("cursorline")
end, { desc = "Toggle Cursorline" })
