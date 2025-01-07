local M = {}

-- TODO: conceal for history (long stdin/commands) ?
-- TODO: Edit history, then run?
-- TODO: Use numbers to select items in history?
-- TODO: Handle window resize

local history = {}
local history_window = nil
local output_window = nil
local output_buf = nil

local function totitle(s, limit)
  limit = limit or 40
  local title = s
  if #title > limit then
    title = title:sub(1, limit - 3) .. '...'
  end
  return title
end

local function close_windows()
  pcall(vim.api.nvim_win_close, history_window, true)
  history_window = nil

  pcall(vim.api.nvim_win_close, output_window, true)
  output_window = nil
end

local function table_to_string(tbl, indent)
  indent = indent or 0
  local result = ""
  for k, v in pairs(tbl) do
    result = result .. string.rep("  ", indent) .. tostring(k) .. " = "
    if type(v) == "table" then
      result = result .. "\n" .. table_to_string(v, indent + 1) .. '\n'
    else
      result = result .. tostring(v) .. "\n"
    end
  end
  if indent == 0 then
    result = result:sub(1, #result - 1)
  end
  return result
end

local function create_buf(lines)
  if type(lines) == 'string' then
    lines = vim.split(lines, '\n')
  end
  local buf = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(buf)
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, lines)
  vim.api.nvim_set_option_value('modifiable', false, { buf = bufnr })
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  return buf
end

local function open_win(buf, title)
  local margin = 0.05
  local row = math.floor(vim.o.lines * margin)
  local col = math.floor(vim.o.columns * margin)
  local win_config = {
    relative = 'editor',
    width = math.floor(vim.o.columns * (1 - margin * 2)),
    height = math.floor(vim.o.lines * (1 - margin * 2)),
    row = row,
    col = col,
    style = 'minimal',
    border = 'rounded',
  }
  if title then
    win_config.title = title
    win_config.title_pos = 'center'
  end
  return vim.api.nvim_open_win(buf, true, win_config)
end

local function goto_file_line_col()
  local line = vim.fn.getline('.')

  local patterns = {
    '^(.-):(%d+):(%d+):.*',  -- Multiple
    '^  File "([^"]+)", line (%d+), in .*',  -- Python
    '^--> (.*):(%d+):(%d+)',  -- Rust
  }

  for _, pattern in ipairs(patterns) do
    local file, line_num, col_num = string.match(line, pattern)
    if file and vim.fn.filereadable(file) == 1 then
      col_num = col_num or '1'
      close_windows()
      vim.cmd("silent edit " .. file)
      vim.fn.cursor(tonumber(line_num), tonumber(col_num))
      return
    end
  end
end

local function run(command, stdin)
  command = vim.fn.trim(command)

  if command == "" then
    vim.notify("[Exec] Empty command", vim.log.levels.WARN)
    return
  end

  stdin = stdin or ""

  close_windows()
  pcall(vim.api.nvim_buf_delete, output_buf, { force = true })
  output_buf = nil

  if vim.o.autowrite then
    pcall(function(x) vim.cmd(x) end, "silent wall")  -- pcall in case buffer isn't writable... just ignore
  end

  local output = vim.fn.system(command, stdin)

  output_buf = create_buf(output)
  vim.keymap.set('n', '<cr>', goto_file_line_col, { buffer = vim.fn.bufnr(output_buf) })

  output_window = open_win(output_buf, totitle(command))

  local new_history = { { command = command, stdin = stdin } }
  for i=1,#history do
    if #new_history == 10 then
      break
    end
    if not (command == history[i].command and stdin == history[i].stdin) then
      table.insert(new_history, history[i])
    end
  end
  history = new_history
end

M.run = function()
  local command_text = ""
  local stdin_text = ""
  if #history > 0 then
    command_text = history[1].command
    stdin_text = history[1].stdin
  end
  local command = vim.fn.input("Command: ", command_text)
  local stdin = vim.fn.input("Stdin: ", stdin_text)

  run(command, stdin)
end

M.run_last = function()
  if #history == 0 then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  run(history[1].command, history[1].stdin)
end

M.history = function()
  close_windows()

  local buf = create_buf(table_to_string(history))
  local bufnr = vim.fn.bufnr(buf)
  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = bufnr })

  vim.keymap.set('n', '<cr>', function()
    local line = vim.fn.line('.')
    local item = math.floor(line / 4 - 0.1) + 1
    run(history[item].command, history[item].stdin)
  end, { buffer = bufnr })

  history_window = open_win(buf, 'History')
end

M.show_last = function()
  if not output_buf then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  close_windows()
  output_window = open_win(output_buf, totitle(history[1].command))
end

M.setup = function()
end

return M
