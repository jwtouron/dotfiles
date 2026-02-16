local function toggle_oil()
  if vim.bo.filetype == 'oil' then
    require('oil').close()
  else
    require('oil').open()
  end
end

local function feedkeys(keys)
  local escaped_keys = vim.api.nvim_replace_termcodes(keys, true, false, true)
  vim.api.nvim_feedkeys(escaped_keys, 'm', true)
end

local function grep_file()
  local oil = require("oil")
  local dir = oil.get_current_dir()
  local entry = oil.get_cursor_entry().name
  if entry == ".." then
    entry = ""
  end
  local path = vim.fn.fnamemodify(dir, ':p') .. entry
  feedkeys(':grep ' .. path .. "<Home><Right><Right><Right><Right>")
  require('oil').close()
end

local keymaps = {
  ["g!"] = {
    desc = "Execute ! on file",
    callback = function()
      local oil = require("oil")
      local dir = oil.get_current_dir()
      local entry = oil.get_cursor_entry().name
      local path = vim.fn.fnamemodify(dir, ':p') .. entry
      feedkeys(':! ' .. path .. "<Home><Right>")
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
  ["gg"] = {
    desc = "Execute grep on file",
    callback = grep_file,
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

return {
  'stevearc/oil.nvim',
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "CmdlineEnter",
  cmd = "Oil",
  keys = { { "<leader>o", toggle_oil, desc = "Oil" } },
  opts = {
    columns = {},
    delete_to_trash = true,
    float = { padding = 4 },
    keymaps = keymaps,
    lsp_file_methods = { enabled = false },
    view_options = { show_hidden = true },
  },
}
