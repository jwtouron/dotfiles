-- TODO: conceal for history (long stdin/commands) ?
-- TODO: Edit history, then run?
-- TODO: Use numbers to select items in history?
-- TODO: Handle window resize

local window_margin = 0.05

local function calculate_window_width()
  return math.floor(vim.o.columns * (1 - window_margin * 2))
end

local function win_is_valid(win)
  return win and vim.api.nvim_win_is_valid(win)
end

local function win_close(win, force)
  vim.api.nvim_win_close(win[1], force)
  win[1] = nil
end

local function start_job(command, on_line, on_exit, stdin)
  local opts = {}

  if on_line then
    local new_handler = function()
      local line = ''
      return function(_, data)
        on_line(line .. data[1])
        for i = 2, #data - 1 do
          on_line(data[i])
        end
        line = data[#data]
      end
    end

    opts.on_stdout = new_handler()
    opts.on_stderr = new_handler()
    opts.stderr_buffered = false
    opts.stdout_buffered = false
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
  local row = math.floor(vim.o.lines * window_margin)
  local col = math.floor(vim.o.columns * window_margin)
  local win_config = {
    relative = 'editor',
    width = calculate_window_width(),
    height = math.floor(vim.o.lines * (1 - window_margin * 2)),
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
  local patterns = {
    '^(.-):(%d+):(%d+):.*',  -- Multiple
    '^  File "([^"]+)", line (%d+), in .*',  -- Python
    '^--> (.*):(%d+):(%d+)',  -- Rust
  }

  local line = vim.fn.getline('.')

  for _, pattern in ipairs(patterns) do
    local file, line_num, col_num = string.match(line, pattern)
    if file and vim.fn.filereadable(file) == 1 then
      col_num = col_num or '1'
      return function()
        vim.cmd("silent edit " .. file)
        vim.fn.cursor(tonumber(line_num), tonumber(col_num))  -- luacheck: ignore param-type-mismatch
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

function Job.run(command, stdin)
  -- Check input

  command = vim.fn.trim(command)
  if command == "" then
    error("[Exec] Empty command")
  end

  stdin = stdin or ""

  -- Initialize instance

  local self = setmetatable({ command = command, stdin = stdin}, Job)

  -- Setup buffer

  self.buffer = create_buf()
  local bufnr = vim.fn.bufnr(self.buffer)
  vim.keymap.set('n', '<C-c>', function() self:cancel() end, { buffer = bufnr })
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

  self.job_id = start_job(command, on_line, on_exit, stdin)
  self.status = 'Running'

  -- Open window

  self.window = open_win(self.buffer, self:window_title())

  return self
end

function Job:cancel()
  if self.job_id then
    vim.fn.jobstop(self.job_id)
    self.job_id = nil
  end
end

function Job:set_window_title()
  if self.window then
    pcall(vim.api.nvim_win_set_config, self.window, { title = self:window_title(), title_pos = 'center' })
  end
end

function Job:window_title()
  return totitle(self.status .. ': ' .. self.command, math.floor(calculate_window_width() / 2))
end

function Job:open_window()
  if not win_is_valid(self.window) then
    self.window = open_win(self.buffer, self:window_title())
  end
end

function Job:close_window()
  if win_is_valid(self.window) then
    win_close({ self.window }, true)
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
  if not win_is_valid(self.window) then
    local buf = create_buf(table_to_string(self.jobs))
    local bufnr = vim.fn.bufnr(buf)
    vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = bufnr })

    vim.keymap.set('n', '<cr>', function()
      local line = vim.fn.line('.')
      local item = math.floor(line / 8 - 0.1) + 1
      self:close_window()
      local job = Job.run(self.jobs[item].command, self.jobs[item].stdin)
      self:add(job)
    end, { buffer = bufnr })

    self.window = open_win(buf, 'History')
  end
end

function History:close_window()
  if win_is_valid(self.window) then
    win_close({ self.window }, true)
  end
end

function History:add(job)
  local new_jobs = { job }
  for i = 1, #self.jobs do
    if #self.jobs == 10 then
      break
    end
    if not (job.command == self.jobs[i].command and job.stdin == self.jobs[i].stdin) then
      table.insert(new_jobs, self.jobs[i])
    end
  end
  self.jobs = new_jobs
end

function History:most_recent()
  return self.jobs[1]
end

--------------------------------------------------------------------------------
--- Module
--------------------------------------------------------------------------------

local history = History.new()

local M = {}

local function run_new(command, stdin)
  command = vim.fn.trim(command)
  if command == "" then
    vim.notify("[Exec] Empty command", vim.log.levels.WARN)
    return
  end
  stdin = stdin or ""

  local most_recent = history:most_recent()

  if most_recent then
    most_recent:close_window()
    most_recent:cancel()
  end
  history:close_window()

  if vim.o.autowrite then
    vim.cmd("silent! wall")
  end

  local job = Job.run(command, stdin)
  history:add(job)
end

M.run = function()
  local command_text = ""
  local stdin_text = ""
  local most_recent = history:most_recent()
  if most_recent then
    command_text = most_recent.command
    stdin_text = most_recent.stdin
  end

  local ok, command = pcall(vim.fn.input, "Command: ", command_text)
  if not ok then return end

  local ok, stdin = pcall(vim.fn.input, "Stdin: ", stdin_text)
  if not ok then return end

  run_new(command, stdin)
end

M.run_last = function()
  local most_recent = history:most_recent()
  if not most_recent then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  run_new(most_recent.command, most_recent.stdin)
end

M.history = function()
  history:open_window()
end

M.show_last = function()
  local most_recent = history:most_recent()
  if not most_recent then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  history:close_window()
  most_recent:open_window()
end

M.setup = function()
end

return M
