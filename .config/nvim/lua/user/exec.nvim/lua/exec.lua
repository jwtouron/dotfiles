-- Helper functions

local function delete_buffer(self)
  if self.buffer ~= nil and vim.api.nvim_buf_is_valid(self.buffer) then
    vim.api.nvim_buf_delete(self.buffer, { force = true })
    self.buffer = nil
  end
end

local function win_is_valid(window)
  return window ~= nil and vim.api.nvim_win_is_valid(window)
end

local function close_window(self)
  if win_is_valid(self.window) then
    vim.api.nvim_win_close(self.window, { force = true })
    self.window = nil
  end
end

local function create_buffer_with_content(content)
  if type(content) == 'string' then
    content = vim.split(content, '\n')
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, content)
  return buf
end

local function create_window(buf, title)
  title = title or 'Done.'
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
    title = title,
    title_pos = 'center',
  }
  return vim.api.nvim_open_win(buf, true, win_config)
end

-- Exec

local Exec = {}

function Exec:new()
  local o = {}
  setmetatable(o, self)
  self.__index = self
  return o
end

function Exec:run(command)
  self:close_window()
  self:delete_buffer()
  self:stop_job()

  local i, j = command:find('^ *!')
  if i then
    local buf = vim.api.nvim_create_buf(false, true)
    local bufnr = vim.fn.bufnr(buf)

    vim.keymap.set('n', '<C-c>', function()
      self:stop_job()
      self.window_title = 'Aborted.'
    end,
    { buffer = bufnr })

    local function append_line(line)
      vim.api.nvim_buf_set_option(bufnr, 'modifiable', true)
      vim.api.nvim_buf_set_lines(buf, -1, -1, true, { line })
      vim.api.nvim_buf_set_option(bufnr, 'modifiable', false)
    end

    local stdout_line = ''
    local stderr_line = ''
    local opts = {
      on_stdout = function(_, data, src)
        if #data == 1 and data[1] == '' then
          -- EOF
          return
        end
        if src == 'stdout' then
          stdout_line = stdout_line .. data[1]
          append_line(stdout_line)
          stdout_line = data[#data]
        elseif src == 'stderr' then
          stderr_line = stderr_line .. data[1]
          append_line(stderr_line)
          stderr_line = data[#data]
        else
          vim.api.nvim_err_writeln("[Exec] Unknown output source: " .. src)
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
    self.job = vim.fn.jobstart(string.sub(command, j + 1), opts)

    self.window_title = 'Running...'
    self.buffer = buf
  else
    local contents = vim.fn.execute(command)
    self.buffer = create_buffer_with_content(contents)
  end

  local bufnr = vim.fn.bufnr(self.buffer)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.api.nvim_buf_set_option(bufnr, 'modifiable', false)

  self:open_window()
end

Exec.close_window = close_window

Exec.delete_buffer = delete_buffer

function Exec:has_run()
  return self.buffer ~= nil
end

function Exec:is_open()
  return win_is_valid(self.window)
end

function Exec:open_window()
  self.window = create_window(self.buffer, self.window_title)
end

function Exec:stop_job()
  if self.job ~= nil then
    vim.fn.jobstop(self.job)
    self.job = nil
  end
end

-- Exec history

local ExecHistory = {}

function ExecHistory:new(n)
  local o = { history = {} }
  n = n or 25
  for i = 1, n do
    o.history[i] = ''
  end
  setmetatable(o, self)
  self.__index = self
  return o
end

function ExecHistory:open()
  self:close_window()

  self:delete_buffer()
  self.buffer = create_buffer_with_content(self.history)
  local bufnr = vim.fn.bufnr(self.buffer)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.keymap.set('n', '<cr>', function() vim.cmd("Exec " .. vim.fn.getline('.')) end, { buffer = bufnr })

  self.window = create_window(self.buffer)
end

function ExecHistory:run_last_command()
  if self.history[1] ~= '' then
    vim.cmd("Exec " .. self.history[1])
  else
    vim.cmd [[echo '[Exec] Empty history!']]
  end
end

function ExecHistory:update(command)
  local move = command
  for i = 1, #self.history do
    local tmp = self.history[i]
    self.history[i] = move
    if tmp == command then
      return
    end
    move = tmp
  end
end

function ExecHistory:is_open()
  return win_is_valid(self.window)
end

ExecHistory.close_window = close_window

ExecHistory.delete_buffer = delete_buffer

-- Module

local M = {}
local exec = Exec:new()
local exec_history = ExecHistory:new()

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
  exec_history:close_window()
  exec:run(command)
  exec_history:update(command)
end

M.exec_history = function()
  if exec_history:is_open() then
    return
  end
  exec:close_window()
  exec_history:open()
end

M.exec_last_command = function()
  exec_history:run_last_command()
end

M.toggle_output = function()
  if not exec:has_run() then
    vim.cmd [[echo '[Exec] Empty history!']]
    return
  end

  if exec:is_open() then
    exec:close_window()
  else
    exec:open_window()
  end
end

return M
