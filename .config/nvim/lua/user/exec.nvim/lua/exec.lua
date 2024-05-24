local function write_err(msg)
  vim.api.nvim_err_writeln('[Exec] ' .. msg)
end

-- Window

local Window = {}
Window.__index = Window

function Window:window_is_valid()
  return self.window ~= nil and vim.api.nvim_win_is_valid(self.window)
end

function Window:close_window()
  if self:window_is_valid() then
    vim.api.nvim_win_close(self.window, true)
    self.window = nil
  end
end

function Window:buffer_is_valid()
  return self.buffer ~= nil and vim.api.nvim_buf_is_valid(self.buffer)
end

function Window:delete_buffer()
  if self:buffer_is_valid() then
    vim.api.nvim_buf_delete(self.buffer, { force = true })
    self.buffer = nil
  end
end

function Window:close()
  self:close_window()
  self:delete_buffer()
end

function Window:open_window()
  if not self:buffer_is_valid() then
    error("Cannot open window: Buffer is invalid!")
  end
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
  if self.window_title then
    win_config.title = self.window_title
    win_config.title_pos = 'center'
  end
  self.window = vim.api.nvim_open_win(self.buffer, true, win_config)
end

-- Exec

Exec = setmetatable({}, {__index = Window})

function Exec:new()
  return setmetatable({}, {__index = self})
end

function Exec:stop_job()
  if self.jobid ~= nil then
    vim.fn.jobstop(self.jobid)
    self.jobid = nil
  end
end

function Exec:close()
  Window.close(self)
  self:stop_job()
end

function Exec:run(command)
  local i, j = command:find('^ *!')
  if i then
    self:_run_async(string.sub(command, j + 1))
  else
    self:_run_sync(command)
  end
end

function Exec:_run_async(command)
  local buf = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(buf)

  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.keymap.set('n', '<C-c>', function()
    self:stop_job()
    self.window_title = 'Aborted.'
  end,
  { buffer = bufnr })

  local function append_line(line)
    vim.api.nvim_set_option_value('modifiable', true, { buf = bufnr })
    vim.api.nvim_buf_set_lines(buf, -1, -1, true, { line })
    vim.api.nvim_set_option_value('modifiable', false, { buf = bufnr })
  end

  local stdout_line = ''
  local stderr_line = ''
  local opts = {
    on_stdout = function(_, data, src)
      if src == 'stdout' then
        if #data == 1 and data[1] == '' then
          append_line(stdout_line)
        else
          stdout_line = stdout_line .. data[1]
          append_line(stdout_line)
          stdout_line = data[#data]
        end
      elseif src == 'stderr' then
        if #data == 1 and data[1] == '' then
          append_line(stderr_line)
        else
          stderr_line = (stderr_line or '') .. data[1]
          append_line(stderr_line)
          stderr_line = data[#data]
        end
      else
        error("Unknown output source: " .. src)
        return
      end
      for ii = 2, #data - 1 do
        append_line(data[ii])
      end
    end,
    on_exit = function()
      if self.window_title == 'Running...' then
        self.window_title = 'Done.'
      end
      vim.api.nvim_win_set_config(0, { title = self.window_title, title_pos = 'center', })
    end,
    stderr_buffered = false,
    stdout_buffered = false,
  }
  opts.on_stderr = opts.on_stdout
  self.jobid = vim.fn.jobstart(command, opts)

  self.buffer = buf
  self.window_title = 'Running...'
end

function Exec:_run_sync(command)
  local output = vim.fn.execute(command)
  local content = vim.split(output, '\n')
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, content)
  local bufnr = vim.fn.bufnr(buf)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.api.nvim_set_option_value('modifiable', false, { buf = bufnr })
  self.buffer = buf
  self.window_title = 'Done.'
end

-- Exec History

ExecHistory = setmetatable({}, {__index = Window})

function ExecHistory:new()
  local o = { history = {}, window_title = 'Exec History', }
  return setmetatable(o, {__index = self})
end

function ExecHistory:add_command(command)
  local new_history = { command }
  for _, c in pairs(self.history) do
    if c ~= command and not c:find('^ *$') then
      table.insert(new_history, c)
    end
  end
  self.history = new_history
end

function ExecHistory:update_from_buffer(first)
  local new_history = {}
  if first then
    new_history[1] = first
  end
  for _, c in pairs(vim.api.nvim_buf_get_lines(self.buffer, 0, -1, false)) do
    if c ~= first and not c:find('^ *$') then
      table.insert(new_history, c)
    end
  end
  self.history = new_history
end

function ExecHistory:display(on_select)
  local buf = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(buf)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, self.history)
  self.buffer = buf

  vim.keymap.set(
    'n',
    'q',
    function()
      self:update_from_buffer()
      self:close_window()
    end,
    { buffer = bufnr }
  )

  vim.keymap.set(
    'n',
    '<cr>',
    function()
      local command = vim.fn.getline('.')
      self:update_from_buffer(command)
      self:close_window()
      on_select(command)
    end,
    { buffer = bufnr }
  )

  Window.open_window(self)
end

-- Module

local exec = Exec:new()
local exec_history = ExecHistory:new()

M = {}

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
  exec:close()
  exec_history:close()
  exec:run(command)
  exec_history:add_command(command)
  exec:open_window()
end

M.exec_history = function()
  if exec_history:window_is_valid() then
    return
  end
  exec:close()
  exec_history:display(function(command) M.exec(command) end)
end

M.exec_last_command = function()
  if exec_history.history[1] then
    M.exec(exec_history.history[1])
  else
    write_err('Empty history!')
  end
end

M.toggle_output = function()
  if not exec:buffer_is_valid() then
    write_err('Empty history!')
    return
  end
  if exec:window_is_valid() then
    exec:close_window()
  else
    exec:open_window()
  end
end

return M
