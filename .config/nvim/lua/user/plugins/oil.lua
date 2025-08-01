local function toggle_oil()
  if vim.bo.filetype == 'oil' then
    require('oil').close()
  else
    require('oil').open()
  end
end

local function grep_file()
  local oil = require("oil")
  local dir = oil.get_current_dir()
  local entry = oil.get_cursor_entry().name
  local path = vim.fn.fnamemodify(dir, ':p') .. entry
  local keys = vim.api.nvim_replace_termcodes(':grep ' .. path .. "<Home><Right><Right><Right><Right>", true, false, true)
  vim.api.nvim_feedkeys(keys, 'm', true)
end

local function feedkeys(keys)
  local escaped_keys = vim.api.nvim_replace_termcodes(keys, true, false, true)
  vim.api.nvim_feedkeys(escaped_keys, 'm', true)
end

return {
  'stevearc/oil.nvim',
  enabled = false,
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "CmdlineEnter",
  cmd = "Oil",
  keys = { { "<leader>o", toggle_oil, desc = "Oil" } },
  opts = {
    delete_to_trash = true,
    float = { padding = 4 },
    keymaps = {
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
              require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
            else
              require("oil").set_columns({ "icon" })
            end
          end
        end)()
      },
      ["gs"] = {
        desc = "Execute grep on file",
        callback = grep_file,
      },
      ["gG"] = {
        desc = "Execute grep on file",
        callback = grep_file,
      },
      ["gt"] = {
        desc = "Open terminal in directory",
        callback = function()
          vim.cmd("term")
          vim.cmd("startinsert")
          -- feedkeys('Acd ' .. dir .. '<Cr> ' .. path .. '<C-a>')
        end,
      },
      ["gT"] = {
        desc = "Open terminal in directory",
        callback = function()
          local oil = require("oil")
          local dir = oil.get_current_dir()
          local entry = oil.get_cursor_entry().name
          local path = vim.fn.fnamemodify(dir, ':p') .. entry
          vim.cmd("term")
          feedkeys('Acd ' .. dir .. '<Cr> ' .. path .. '<C-a>')
        end,
      },
    },
    view_options = { show_hidden = true },
  },
}
