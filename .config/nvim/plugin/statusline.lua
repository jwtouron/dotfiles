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

local diag_counts = function()
  local winid = vim.g.statusline_winid

  local b = vim.api.nvim_win_get_buf(winid)
  local d = vim.diagnostic

  local err = #d.get(b, { severity = d.severity.ERROR })
  local warn = #d.get(b, { severity = d.severity.WARN })
  local info = #d.get(b, { severity = d.severity.INFO })
  local hint = #d.get(b, { severity = d.severity.HINT })

  if err == 0 and warn == 0 and info == 0 and hint == 0 then
    return ""
  end

  local parts = {}
  if err  > 0 then parts[#parts+1] = ("%%#DiagnosticError#󰅚 %d%%*"):format(err) end
  if warn > 0 then parts[#parts+1] = ("%%#DiagnosticWarn#󰀪 %d%%*"):format(warn) end
  if info > 0 then parts[#parts+1] = ("%%#DiagnosticInfo#󰌶 %d%%*"):format(info) end
  if hint > 0 then parts[#parts+1] = ("%%#DiagnosticHint# %d%%*"):format(hint) end
  local diags = table.concat(parts, " ")
  if diags ~= '' then
     diags = diags .. " ·"
   end
  return diags
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

    diag_counts(),
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
