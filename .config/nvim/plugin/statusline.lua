if true then return end

local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

local getcwd = function()
  local cwd = vim.fn.fnamemodify(vim.fn.getcwd(), ':~')
  local parts = vim.split(cwd, '/')
  for i = 1, #parts - 1 do
    if parts[i] == '~' then
      -- Do nothing
    elseif string.sub(parts[i], 1, 1) == '.' then
      parts[i] = string.sub(parts[i], 1, 2)
    else
      parts[i] = string.sub(parts[i], 1, 1)
    end
  end
  return vim.fn.join(parts, '/')
end

local filetype = function()
  local ft = vim.opt.filetype:get()
  if ft ~= '' then
    ft = ft .. " ·"
  end
  return ft
end

local line_percent = function()
  local winid = vim.g.statusline_winid
  local bufnr = vim.api.nvim_win_get_buf(winid)
  local curr = vim.api.nvim_win_get_cursor(winid)[1]
  local last = vim.api.nvim_buf_line_count(bufnr)
  if curr == 1 then
    return 'Top'
  elseif curr == last then
    return 'Bot'
  else
    return string.format('%.f%%%%', 100.0 * curr / last)
  end
end

_G.my_statusline = function()
  return table.concat({
    getcwd(),
    "·",
    '%f',
    '%h%m%r',

    '%=',

    vim.diagnostic.status(),
    filetype(),
    line_percent(),
    '%l:%v',
  }, " ")
end

vim.opt.statusline = '%!v:lua.my_statusline()'

vim.api.nvim_create_autocmd("DiagnosticChanged", {
  group = augroup,
  callback = function() vim.cmd.redrawstatus() end,
})
