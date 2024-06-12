local function toggle_oil()
  if vim.bo.filetype == 'oil' then
    require('mini.bufremove').wipeout()
    -- vim.api.nvim_buf_delete(0, { force = true })
  else
    require('oil').open()
  end
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
  },
}
