local function toggle_oil()
  if vim.bo.filetype == 'oil' then
    -- vim.cmd("b#|bw#")
    -- require('mini.bufremove').wipeout()
    vim.api.nvim_buf_delete(0, { force = true })
  else
    require('oil').open()
  end
end

local detail = false

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
        callback = function()
          detail = not detail
          if detail then
            require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
          else
            require("oil").set_columns({ "icon" })
          end
        end,
      },
      ["gg"] = {
        desc = "Execute grep on file",
        callback = function()
          local oil = require("oil")
          local dir = oil.get_current_dir()
          local entry = oil.get_cursor_entry().name
          local path = vim.fn.fnamemodify(dir, ':p') .. entry
          local keys = vim.api.nvim_replace_termcodes(':grep ' .. path .. "<Home><Right><Right><Right><Right>", true, false, true)
          vim.api.nvim_feedkeys(keys, 'm', true)
        end,
      },
    },
    view_options = { show_hidden = true },
  },
}
