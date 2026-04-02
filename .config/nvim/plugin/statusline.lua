if true then return end

local function status_win()
  return vim.g.statusline_winid or vim.api.nvim_get_current_win()
end

local function status_buf()
  return vim.api.nvim_win_get_buf(status_win())
end

local function getcwd()
  local cwd = vim.fn.fnamemodify(vim.fn.getcwd(status_win()), ":~")
  local parts = vim.split(cwd, "/", { plain = true })

  for i = 1, #parts - 1 do
    if parts[i] == "~" then
      -- keep as-is
    elseif parts[i]:sub(1, 1) == "." then
      parts[i] = parts[i]:sub(1, 2)
    else
      parts[i] = parts[i]:sub(1, 1)
    end
  end

  return table.concat(parts, "/")
end

local function filepath()
  local name = vim.api.nvim_buf_get_name(status_buf())
  if name == "" then
    return "[No Name]"
  end
  return vim.fn.fnamemodify(name, ":~:.")
end

local function flags()
  local winid = status_win()
  local bufnr = status_buf()
  local s = ""

  if vim.bo[bufnr].buftype == "help" then
    s = s .. "h"
  end
  if vim.wo[winid].previewwindow then
    s = s .. "w"
  end
  if vim.bo[bufnr].modified then
    s = s .. "+"
  end
  if vim.bo[bufnr].readonly then
    s = s .. "r"
  end

  return s
end

local function progress()
  if not package.loaded["vim.ui"] then
    return ""
  end
  if vim.api.nvim_get_current_win() ~= tonumber(vim.g.actual_curwin or -1) then
    return ""
  end
  return vim.ui.progress_status()
end

local function busy()
  return vim.o.busy > 0 and "◐" or ""
end

local function diagnostics()
  return vim.diagnostic.status(status_buf())
end

local function filetype()
  local ft = vim.bo[status_buf()].filetype
  return ft ~= "" and (ft .. " ·") or ""
end

local function line_percent()
  local winid = status_win()
  local bufnr = status_buf()
  local curr = vim.api.nvim_win_get_cursor(winid)[1]
  local last = vim.api.nvim_buf_line_count(bufnr)

  if curr == 1 then
    return "Top"
  elseif curr == last then
    return "Bot"
  else
    return string.format("%.f%%%%", 100 * curr / last)
  end
end

local function position()
  local winid = status_win()
  local pos = vim.api.nvim_win_get_cursor(winid)
  return string.format("%d:%d", pos[1], pos[2] + 1)
end

local function compact(xs)
  return vim.tbl_filter(function(x)
    return x ~= nil and x ~= ""
  end, xs)
end

_G.my_statusline = function()
  local left = compact({
    getcwd(),
    "·",
    filepath(),
    flags(),
  })

  local right = compact({
    progress(),
    busy(),
    diagnostics(),
    filetype(),
    line_percent(),
    position(),
  })

  return table.concat(left, " ") .. " %=" .. table.concat(right, " ")
end

vim.opt.statusline = "%!v:lua.my_statusline()"

-- local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})
--
-- vim.api.nvim_create_autocmd("DiagnosticChanged", {
--   group = augroup,
--   callback = function()
--     vim.cmd.redrawstatus()
--   end,
-- })
