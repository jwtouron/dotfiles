local function toggle_oil()
  if vim.bo.filetype == 'oil' then
    require('mini.bufremove').wipeout()
    -- vim.api.nvim_buf_delete(0, { force = true })
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
    },
  }
}
