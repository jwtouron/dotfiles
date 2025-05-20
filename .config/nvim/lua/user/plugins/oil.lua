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

return {
  'stevearc/oil.nvim',
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
          local keys = vim.api.nvim_replace_termcodes(':! ' .. path .. "<Home><Right>", true, false, true)
          vim.api.nvim_feedkeys(keys, 'm', true)
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
    },
    view_options = { show_hidden = true },
  },
}
