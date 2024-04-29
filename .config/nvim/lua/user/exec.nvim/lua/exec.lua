local exec_window = nil
local exec_history = {}
local exec_history_window = nil

for i = 1, 10 do
    exec_history[i] = ''
end

local function update_history(command)
  local move = command
  for i = 1, #exec_history do
    local tmp = exec_history[i]
    exec_history[i] = move
    if tmp == command then
      return
    end
    move = tmp
  end
end

local function close_window_if_valid(window)
  if window and vim.api.nvim_win_is_valid(window) then
    vim.fn.win_execute(window, 'q')
  end
end

local function create_window(buf)
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
  exec_window = vim.api.nvim_open_win(buf, true, win_config)
end

local function create_buffer(contents)
  if type(contents) == 'string' then
    contents = vim.split(contents, '\n')
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, contents)
  return buf
end

local M = {}

M.setup = function()
  vim.api.nvim_create_user_command(
    "Exec",
    function(arg) M.exec(arg.args) end,
    {
      desc = "Run a command with Exec",
      nargs = 1,
    }
  )

  vim.api.nvim_create_user_command(
    "ExecHistory",
    function() M.exec_history() end,
    {
      desc = "Show the history of commands run with Exec",
      nargs = 0,
    }
  )

  vim.api.nvim_create_user_command(
    "ExecLastCommand",
    function() M.exec_last_command() end,
    {
      desc = "Execute the last command run with Exec",
      nargs = 0,
    }
  )
end

M.exec = function(command)
  close_window_if_valid(exec_window)
  close_window_if_valid(exec_history_window)

  local contents = vim.fn.execute(command)
  local buf = create_buffer(contents)
  local bufnr = vim.fn.bufnr(buf)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.api.nvim_buf_set_option(bufnr, 'modifiable', false)

  update_history(command)

  create_window(buf)
end

M.exec_history = function()
  close_window_if_valid(exec_history_window)
  close_window_if_valid(exec_window)

  local buf = create_buffer(exec_history)
  local bufnr = vim.fn.bufnr(buf)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.keymap.set('n', '<cr>', function() M.exec(vim.fn.getline('.')) end, { buffer = bufnr })

  create_window(buf)
end

M.exec_last_command = function()
  if exec_history[1] ~= '' then
    M.exec(exec_history[1])
  else
    vim.cmd [[echo '[Exec] Empty history!']]
  end
end

return M
