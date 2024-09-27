-- vim: set foldenable foldmethod=marker:

local namespace = vim.api.nvim_create_namespace('Exec')
vim.api.nvim_set_hl(namespace, 'Success', { fg = 'green', bg = 'none' })
vim.api.nvim_set_hl(namespace, 'Error', { fg = 'red', bg = 'none' })
vim.api.nvim_set_hl(namespace, 'Stdin', { fg = 'blue', bg = 'none' })

-- {{{1 Job

local function start_job(command, on_line, on_exit, stdin)
  local opts = { on_exit = on_exit }

  if on_line then
    opts.stderr_buffered = false
    opts.stdout_buffered = false

    local stdout_line = ''
    local stderr_line = ''

    opts.on_stdout = function(_, data, src)
      if src == 'stdout' then
        if #data == 1 and data[1] == '' then
          on_line(stdout_line)
        else
          stdout_line = stdout_line .. data[1]
          on_line(stdout_line)
          stdout_line = data[#data]
        end
      elseif src == 'stderr' then
        if #data == 1 and data[1] == '' then
          on_line(stderr_line)
        else
          stderr_line = (stderr_line or '') .. data[1]
          on_line(stderr_line)
          stderr_line = data[#data]
        end
      else
        error("Unknown output source: " .. src)
        return
      end
      for i = 2, #data - 1 do
        on_line(data[i])
      end
    end

    opts.on_stderr = opts.on_stdout
  end

  local job_id = vim.fn.jobstart(command, opts)

  if stdin then
    vim.fn.chansend(job_id, stdin)
    vim.fn.chanclose(job_id, 'stdin')
  end

  return job_id
end

-- {{{1 Exec

local Exec = {}
Exec.__index = Exec

function Exec.run(command, stdin)
  local self = setmetatable({}, Exec)

  self.command = command
  self.stdin = stdin
  self.output = {}

  self.buffer = vim.api.nvim_create_buf(false, true)
  self.bufnr = vim.fn.bufnr(self.buffer)

  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = self.bufnr })

  self.window = self:_open_window("Running...")

  local function on_line(line)
    table.insert(self.output, line)
    pcall(vim.api.nvim_buf_set_lines, self.buffer, -1, -1, true, { line })
  end
  local function on_exit(_, exit_code)
    self.exit_code = exit_code
    if not self.aborted then
      vim.api.nvim_win_set_config(0, { title = 'Done.', title_pos = 'center', })
    end
  end
  self.job_id = start_job(command, on_line, on_exit, stdin)

  vim.keymap.set('n', '<C-c>', function()
    vim.fn.jobstop(self.job_id)
    vim.api.nvim_win_set_config(0, { title = 'Aborted.', title_pos = 'center', })
    self.aborted = true
  end)

  return self
end

function Exec:close()
  if self.job_id then
    vim.fn.jobstop(self.job_id)
    self.job_id = nil
  end

  if self.window and vim.api.nvim_win_is_valid(self.window) then
    vim.api.nvim_win_close(self.window, true)
    self.window = nil
  end

  if self.buffer and vim.api.nvim_buf_is_valid(self.buffer) then
    vim.api.nvim_buf_delete(self.buffer, {force = true })
    self.buffer = nil
  end
end

function Exec:toggle_window()
  if self.window ~= nil and vim.api.nvim_win_is_valid(self.window) then
    vim.api.nvim_win_close(self.window, true)
    self.window = nil
  else
    local title = 'Running...'
    if self.aborted then
      title = 'Aborted.'
    elseif self.exit_code then
      title = 'Done.'
    end
    self.window = self:_open_window(title)
  end
end

function Exec:_open_window(title)
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

  return vim.api.nvim_open_win(self.buffer, true, win_config)
end

-- {{{1 History

local History = {}
History.__index = History

function History.new(capacity)
  local self = setmetatable({ execs = {}, windows = {}, }, History)
  self.capacity = capacity or 25
  return self
end

function History:add(exec)
  self.execs[self.capacity] = nil
  table.insert(self.execs, 1, exec)
end

function History:toggle_window()
  if self.windows.execs ~= nil and vim.api.nvim_win_is_valid(self.windows.execs) then
    self:close_windows()
  else
    self:open_windows()
  end
end

function History:open_windows()
  self:open_window_execs()
end

function History:open_window_execs()
  local buffer = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(buffer)

  local content = {}
  for _, exec in ipairs(self.execs) do
    local line = ''

    if exec.stdin then
      line = line .. '● '
    else
      line = line .. '  '
    end

    if exec.exit_code == 0 then
      line = line .. '✓ '
    elseif exec.exit_code ~= nil then
      line = line .. '✗ '
    else
      line = line .. '  '
    end

    line = line .. exec.command

    table.insert(content, line)
  end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, true, content)

  vim.api.nvim_buf_add_highlight(bufnr, namespace, 'Stdin', 0, 0, -1)
  -- vim.api.nvim_buf_set_extmark(buffer, namespace, 0, 0, { hl_group = 'stdin', })
  -- vim.api.nvim_buf_set_extmark(buffer, namespace, 1, 0, { hl_group = 'stdin', hl_mode = 'combine', })
  -- vim.api.nvim_buf_set_extmark(buffer, namespace, (content[#content] or 1) - 1, 0, { hl_group = 'stdin', hl_mode = 'combine', })

  local margin = 0.05
  local row = math.floor(vim.o.lines * margin)
  local col = math.floor(vim.o.columns * margin)
  local config = {
    relative = 'editor',
    width = math.floor(vim.o.columns * (1 - margin * 2)),
    height = math.floor(vim.o.lines * (1 - margin * 2)),
    row = row,
    col = col,
    style = 'minimal',
    border = 'rounded',
  }
  self.windows.execs = vim.api.nvim_open_win(buffer, true, config)

  vim.api.nvim_buf_set_option(buffer, "bufhidden", "wipe")
end

function History:close_windows()
  for _, window in ipairs(self.windows) do
    if window ~= nil and vim.api.nvim_win_is_valid(window) then
      vim.api.nvim_win_close(window, true)
    end
  end
  self.windows = {}
end

-- {{{1 Module

local history = History.new()
local M = {}

M.setup = function()
end

M.run = function(command, stdin)
  if history.execs[1] then
    history[1]:close()
  end

  history:add(Exec.run(command, stdin))
end

M.rerun = function()
  if history[1] then
    local exec =  Exec.run(history[1].command, history[1].stdin)
    history[1]:close()
    history[1] = exec
  else
    vim.api.nvim_err_writeln("[Exec] Empty history!")
  end
end

M.toggle_output = function()
  if history[1] then
    history[1]:toggle_window()
  else
    vim.api.nvim_err_writeln("[Exec] Empty history!")
  end
end

vim.keymap.set('n', '<space>ee', function() M.run('python3', 'print("hello")') end)
vim.keymap.set('n', '<space>er', M.rerun)
vim.keymap.set('n', '<space>eo', M.toggle_output)
vim.keymap.set('n', '<space>eh', function() history:toggle_window() end)

return M
