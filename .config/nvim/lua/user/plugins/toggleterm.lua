local Util = require("user.util")

local function toggleterm(Terminal, cmd, opts)
  local args = {
    cmd = cmd,
    direction = "float",
    hidden = true,
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

local function lazygit(Terminal, get_root)
  return function()
    local opts = nil
    if get_root then
      opts = { dir = Util.get_root() }
    end
    toggleterm(Terminal, "lazygit", opts):open()
  end
end

local function xplr(Terminal, get_root)
  return function()
    local fname = vim.fn.tempname()
    local opts = {
      on_close = function()
        if vim.fn.filereadable(fname) ~= 0 then
          for _, line in ipairs(vim.fn.readfile(fname)) do
            if vim.fn.filereadable(line) ~= 0 then
              vim.cmd.edit(vim.fn.fnameescape(line))
              -- No idea why these options aren't respected.
              vim.opt_local.number = true
              vim.opt_local.relativenumber = true
            end
          end
          vim.fn.delete(fname)
        end
      end,
    }
    if get_root then
      opts.dir = Util.get_root()
    end
    toggleterm(Terminal, "xplr > " .. fname, opts):open()
  end
end

local keys = {
  { "<leader>fm", { xplr, true }, "[F]ile [M]anager (root dir)" },
  { "<leader>fM", { xplr, nil }, "[F]ile [M]anager (cwd)" },
  { "<leader>gg", { lazygit, true }, "Lazygit (root dir)" },
  { "<leader>gG", { lazygit, nil }, "Lazygit (cwd)" },
}

return {
  "akinsho/toggleterm.nvim",
  cmd = { "ToggleTerm", "ToggleTermSendCurrentLine", "ToggleTermSendVisualLines", "ToggleTermSendVisualSelection", },
  keys = function()
    local ret = {}
    for i, key in ipairs(keys) do
      ret[i] = { key[1], nil, desc = key[3] }
    end
    return ret
  end,
  config = function()
    require("toggleterm").setup()
    local Terminal = require("toggleterm.terminal").Terminal
    for _, key in ipairs(keys) do
      vim.keymap.set("n", key[1], key[2][1](Terminal, key[2][2]), { desc = key[3] })
    end
  end,
}
