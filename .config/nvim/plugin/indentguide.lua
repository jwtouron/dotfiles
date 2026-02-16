local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

if true then
  return
end

local ns = vim.api.nvim_create_namespace("MyIndentGuide")

local hl_group_name = "MyIndentGuide"

local function set_hl()
  -- vim.api.nvim_set_hl(0, hl_group_name, { fg = "#5f87af", bold = true, force = true, })
  -- vim.api.nvim_set_hl(0, hl_group_name, { fg = "#6b8fd6", bold = true, force = true, })
  vim.api.nvim_set_hl(0, hl_group_name, { fg = "fg", bold = true, force = true, })
  -- vim.api.nvim_set_hl(0, hl_group_name, { fg = "#7aa2f7", bold = true, force = true, })
end

set_hl()

vim.api.nvim_create_autocmd("ColorScheme", {
  group = augroup,
  callback = function()
    vim.schedule(set_hl)
  end
})

local function show_indent_marker()
  vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)

  if vim.bo[0].buftype ~= "" then
    return
  end

  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local line = vim.fn.getline('.')

  if line:match("^%s*$") then
    return
  end

  local indent = vim.fn.indent('.')

  if indent == 0 then
    return
  end

  local sw = vim.fn.shiftwidth()
  local et = vim.bo[0].expandtab

  local col = math.floor(indent / sw)

  if indent % sw == 0 then
    col = col - 1
  end

  if et then
    col = col * sw
  end

  vim.api.nvim_buf_set_extmark(0, ns, row, col, {
    -- virt_text = { { "▷", hl_group_name } },
    virt_text = { { "▶", hl_group_name } },
    virt_text_pos = "overlay",
  })
end

vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "InsertLeave", "BufEnter" }, {
  group = augroup,
  callback = show_indent_marker,
})
