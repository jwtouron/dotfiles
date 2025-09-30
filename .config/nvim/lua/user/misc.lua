-- Build custom help tags
local docdir = vim.fn.stdpath("config") .. "/doc"
local tagfile = docdir .. "/tags"

if vim.fn.filereadable(tagfile) == 0 then
  vim.schedule(function() vim.cmd("helptags " .. vim.fn.fnameescape(docdir)) end)
end

local M = {}

local function goto_file_line_col()
  local patterns = {
    '^(.-):(%d+):(%d+):.*',                  -- Multiple
    '^(.-):(%d+):.*',                        -- Multiple
    '^--> (.-):(%d+):(%d+)',                 -- Rust
    '^  File "([^"]+)", line (%d+), in .*',  -- Python
  }

  local line = vim.fn.getline('.')

  for _, pattern in ipairs(patterns) do
    local file, line_num, col_num = string.match(line, pattern)
    line_num, col_num = tonumber(line_num), tonumber(col_num)
    if file then
      file = vim.fn.fnameescape(file)
      if vim.fn.filereadable(file) == 1 and line_num then
        col_num = col_num or 1
        vim.cmd("silent close | silent edit " .. file)
        vim.fn.cursor(line_num, col_num)
        return
      end
    end
  end
end

local function open_win(buf)
  local row_margin, col_margin = math.floor(vim.o.lines * 0.1), math.floor(vim.o.columns * 0.1)
  local win = vim.api.nvim_open_win(buf, true, {
    row = row_margin,
    col = col_margin,
    height = vim.o.lines - row_margin * 2 - 2,
    width = vim.o.columns - col_margin * 2 - 2,
    style = 'minimal',
    border = 'rounded',
    relative = 'editor',
  })
  return win
end

M.toggle = function()
  if M.buf and vim.api.nvim_buf_is_valid(M.buf) then
    if M.win and vim.api.nvim_win_is_valid(M.win) then
      vim.api.nvim_win_close(M.win, true)
      M.win = nil
    else
      M.win = open_win(M.buf)
    end
  else
    pcall(vim.api.nvim_win_close, M.win, true)
    M.buf = vim.api.nvim_create_buf(false, true)
    M.win = open_win(M.buf)
    vim.cmd("term")
    vim.cmd("startinsert")
    vim.keymap.set("n", "<cr>", goto_file_line_col, { buffer = vim.fn.bufnr(M.buf) })
  end
end

vim.keymap.set("n", "<leader>t", M.toggle)
