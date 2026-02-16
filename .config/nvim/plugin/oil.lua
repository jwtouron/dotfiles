vim.pack.add({ { src = "https://github.com/stevearc/oil.nvim", } }, { confirm = false })

local function toggle_oil()
  if vim.bo.filetype == 'oil' then
    require('oil').close()
  else
    require('oil').open()
  end
end

local keymaps = {
  ["g:"] = {
    desc = "Open command-line mode with the current entry as input.",
    callback = function()
      local oil = require("oil")
      local dir = oil.get_current_dir()
      local entry = oil.get_cursor_entry().name
      local path = vim.fn.fnamemodify(dir, ':p') .. entry
      vim.api.nvim_input(': ' .. path .. "<Home>")
    end,
  },
  ["gd"] = {
    desc = "Toggle file detail view",
    callback = (function()
      local detail = false
      return function()
        detail = not detail
        if detail then
          require("oil").set_columns({ "permissions", "size", "mtime" })
        else
          require("oil").set_columns({})
        end
      end
    end)()
  },
  ["gt"] = {
    desc = "Open terminal in current Oil directory)",
    callback = function()
      local oil = require("oil")
      local dir = oil.get_current_dir()
      vim.cmd(string.format("term sh -c 'cd %s; exec $SHELL -i'", vim.fn.shellescape(dir)))
      vim.cmd("startinsert")
    end,
  },
}

require('oil').setup {
  columns = {},
  delete_to_trash = true,
  float = { padding = 4 },
  keymaps = keymaps,
  lsp_file_methods = { enabled = false },
  view_options = { show_hidden = true },
}

vim.keymap.set('n', '<leader>o', toggle_oil, { desc = 'Toggle Oil' })
