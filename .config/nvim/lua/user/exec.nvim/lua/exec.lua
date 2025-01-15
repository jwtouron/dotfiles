local M = {}

-- TODO: conceal for history (long stdin/commands) ?
-- TODO: Edit history, then run?
-- TODO: Use numbers to select items in history?
-- TODO: Handle window resize

local function start_job(command, on_line, on_exit, stdin)
  local opts = {}
  if on_line then
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
    opts.stderr_buffered = false
    opts.stdout_buffered = false
    opts.on_stderr = opts.on_stdout
  end
  if on_exit then
    opts.on_exit = on_exit
  end
  local job_id = vim.fn.jobstart(command, opts)
  if stdin then
    vim.fn.chansend(job_id, stdin)
    vim.fn.chanclose(job_id, 'stdin')
  end
  return job_id
end

local function totitle(s, limit)
  limit = limit or 40
  local title = s
  if #title > limit then
    title = title:sub(1, limit - 3) .. '...'
  end
  return title
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
  lines = lines or {}
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
      return function()
        vim.cmd("silent edit " .. file)
        vim.fn.cursor(tonumber(line_num), tonumber(col_num))
      end
    end
  end

  return function() end
end

--------------------------------------------------------------------------------
--- Job
--------------------------------------------------------------------------------

local Job = {}
Job.__index = Job

function Job.run(cmd, stdin)
  -- Check input

  cmd = vim.fn.trim(cmd)
  if cmd == "" then
    error("[Exec] Empty command")
  end

  stdin = stdin or ""


  local self = setmetatable({}, Job)

  -- Setup buffer

  self.buffer = create_buf()
  local bufnr = vim.fn.bufnr(self.buffer)
  vim.keymap.set('n', '<cr>',
  function()
    local goto = goto_file_line_col()
    self:close_window()
    goto()
  end,
  { buffer = bufnr })

  -- Run command

  local on_line = function(line)
    vim.api.nvim_set_option_value('modifiable', true, { buf = bufnr })
    vim.api.nvim_buf_set_lines(self.buffer, -1, -1, true, { line })
    vim.api.nvim_set_option_value('modifiable', false, { buf = bufnr })
  end

  local on_exit = function(_, exit_code)
    if exit_code == 143 then
      self.status = 'Cancelled'
    else
      self.status = 'Done'
    end
    self:set_window_title()
  end

  self.id = start_job(cmd, on_line, on_exit, stdin)

  vim.keymap.set('n', '<C-c>', self.cancel, { buffer = bufnr })

  -- Open window

  self.window = open_win(self.buffer, self:window_title())

  -- Return the instance

  self.command = cmd
  self.stdin = stdin
  self.status = 'Running'

  return self
end

function Job:cancel()
  if self.id then
    vim.fn.jobstop(self.id)
    self.id = nil
  end
end

function Job:set_window_title()
  if self.window then
    vim.api.nvim_win_set_config(self.window, { title = self:window_title(), title_pos = 'center' })
  end
end

function Job:window_title()
  return self.status .. ': ' .. self.command
end

function Job:open_window()
  if not (self.window and vim.api.nvim_win_is_valid(self.window)) then
    self.window = open_win(self.buffer, self:window_title())
  end
end

function Job:close_window()
  if self.window and vim.api.nvim_win_is_valid(self.window) then
    vim.api.nvim_win_close(self.window, true)
    self.window = nil
  end
end

--------------------------------------------------------------------------------
--- History
--------------------------------------------------------------------------------

local History = {}
History.__index = History

function History.new(limit)
  limit = limit or 10
  return setmetatable({ jobs = {}, limit = limit }, History)
end

function History:open_window()
  if not (self.window and vim.api.nvim_win_is_valid(self.window)) then
    local buf = create_buf(table_to_string(self.jobs))
    local bufnr = vim.fn.bufnr(buf)
    vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = bufnr })

    vim.keymap.set('n', '<cr>', function()
      local line = vim.fn.line('.')
      local item = math.floor(line / 4 - 0.1) + 1
      local job = Job.run(self.jobs[item].command, self.jobs[item].stdin)
      self.add(job)
    end, { buffer = bufnr })

    self.window = open_win(buf, 'History')
  end
end

function History:close_window()
  if self.window and vim.api.nvim_win_is_valid(self.window) then
    vim.api.nvim_win_close(self.window, true)
    self.window = nil
  end
end

function History:add(job)
  local new_jobs = { { command = job.command, stdin = job.stdin } }
  for i=1,#self.jobs do
    if #self.jobs == 10 then
      break
    end
    if not (job.command == self.jobs[i].command and job.stdin == self.jobs[i].stdin) then
      table.insert(new_jobs, self.jobs[i])
    end
  end
  self.jobs = new_jobs
end

--------------------------------------------------------------------------------
--- Module
--------------------------------------------------------------------------------

local history = History.new()

local function run(command, stdin)

  -- Cleanup previous things

  if job_id then
    vim.fn.jobstop(job_id)
    job_id = nil
  end

  output_status = nil

  close_windows()
  pcall(vim.api.nvim_buf_delete, output_buf, { force = true })
  output_buf = nil

  if vim.o.autowrite then
    vim.cmd("silent! wall")
  end


  -- Update history

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
end

M.show_last = function()
  if not output_buf then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  close_windows()
  output_window = open_win(output_buf)
  set_output_window_title(history[1].command)
end

M.setup = function()
end

return M
