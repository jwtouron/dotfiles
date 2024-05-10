local globals = {
  exec_window = nil,
  exec_buffer = nil,

  exec_history = {},
  exec_history_window = nil,
}

for i = 1, 25 do
  globals.exec_history[i] = ''
end

local function update_history(command)
  local move = command
  for i = 1, #globals.exec_history do
    local tmp = globals.exec_history[i]
    globals.exec_history[i] = move
    if tmp == command then
      return
    end
    move = tmp
  end
end

local function win_is_valid(window)
  return window ~= nil and vim.api.nvim_win_is_valid(window)
end

local function close_window(window)
  if globals[window] and vim.api.nvim_win_is_valid(globals[window]) then
    vim.api.nvim_win_close(globals[window], { force = true })
    globals[window] = nil
  end
end

local function create_window(buf, window)
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
  globals[window] = vim.api.nvim_open_win(buf, true, win_config)
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
end

M.exec = function(command)
  close_window('exec_window')
  close_window('exec_history_window')

  local contents = vim.fn.execute(command)
  local buf = create_buffer(contents)
  if globals['exec_buffer'] ~= nil then
    vim.api.nvim_buf_delete(globals['exec_buffer'], { force = true })
  end
  globals['exec_buffer'] = buf
  local bufnr = vim.fn.bufnr(buf)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.api.nvim_buf_set_option(bufnr, 'modifiable', false)

  update_history(command)

  create_window(buf, 'exec_window')
end

M.exec_history = function()
  if win_is_valid(globals['exec_history_window']) then
    return
  end

  close_window('exec_window')

  local buf = create_buffer(globals['exec_history'])
  local bufnr = vim.fn.bufnr(buf)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.keymap.set('n', '<cr>', function() M.exec(vim.fn.getline('.')) end, { buffer = bufnr })

  create_window(buf, 'exec_history_window')
end

M.exec_last_command = function()
  if globals['exec_history'][1] ~= '' then
    M.exec(globals['exec_history'][1])
  else
    vim.cmd [[echo '[Exec] Empty history!']]
  end
end

M.show_last_output = function()
  if globals['exec_buffer'] == nil then
    vim.cmd [[echo '[Exec] Empty history!']]
    return
  end

  if win_is_valid(globals['exec_window']) then
    return
  end

  close_window('exec_history_window')
  create_window(globals['exec_buffer'], 'exec_window')
end

return M
