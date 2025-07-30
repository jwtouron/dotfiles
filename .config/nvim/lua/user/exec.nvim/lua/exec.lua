local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

local Autocmds = {}
Autocmds.__index = Autocmds

function Autocmds.new()
  local self = setmetatable({ autocmds = {} }, Autocmds)
  return self
end

---@param autocmd integer
function Autocmds:add(autocmd)
  table.insert(self.autocmds, autocmd)
end

function Autocmds:delete()
  for _, autocmd in ipairs(self.autocmds) do
    vim.api.nvim_del_autocmd(autocmd)
  end
end

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
    if file and vim.fn.filereadable(file) == 1 and line_num then
      col_num = col_num or 1
      vim.cmd("silent q! | silent edit " .. file)
      vim.fn.cursor(line_num, col_num)
      return
    end
  end
end

---@param cmd string
---@param opts { on_line: fun(string)?, on_exit: fun(integer, integer)?, stdin: string? }?
---@return integer
local function jobstart(cmd, opts)
  opts = opts or {}
  local job_opts = { stdin = "pipe" }

  if opts.stdin == nil or opts.stdin == "" then
    opts.stdin = nil
    job_opts.stdin = "null"
  end

  if opts.on_line then
    local new_handler = function()
      local line = ''
      return function(_, data)
        opts.on_line(line .. data[1])
        for i = 2, #data - 1 do
          opts.on_line(data[i])
        end
        line = data[#data]
      end
    end

    job_opts.on_stdout = new_handler()
    job_opts.on_stderr = new_handler()
    job_opts.stdout_buffered = false
    job_opts.stderr_buffered = false
  end

  if opts.on_exit then
    job_opts.on_exit = opts.on_exit
  end

  local job_id = vim.fn.jobstart(cmd, job_opts)

  if opts.stdin then
    vim.fn.chansend(job_id, opts.stdin)
    vim.fn.chanclose(job_id, 'stdin')
  end

  return job_id
end

---@class Job
---@field cmd string
---@field stdin string?
---@field buf integer
---@field job_id integer
---@field status string
---@field win integer?
local Job = {}
Job.__index = Job

---@param cmd string
---@param stdin string?
---@return Job
function Job.start(cmd, stdin)
  local self = setmetatable({ cmd = cmd, stdin = stdin, status = 'Running', }, Job)  ---@type Job

  self.buf = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(self.buf)
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = bufnr })
  vim.api.nvim_set_option_value("modifiable", false, { buf = bufnr })

  local row = 0
  local function on_line(line)
    vim.api.nvim_set_option_value("modifiable", true, { buf = bufnr })
    vim.api.nvim_buf_set_lines(self.buf, row, -1, true, { line })
    vim.api.nvim_set_option_value("modifiable", false, { buf = bufnr })

    if row == 0 then
      row = -1
    end
  end

  local on_exit = function(_, exit_code)
    if exit_code == 143 then
      self.status = 'Cancelled'
    else
      self.status = 'Done'
    end
    pcall(vim.api.nvim_win_set_config, self.win, { title = self.status })
  end

  self.job_id = jobstart(cmd, { on_line = on_line, on_exit = on_exit, stdin = stdin })

  vim.keymap.set("n", "<C-c>", function() print('jobstop:', vim.fn.jobstop(self.job_id)) end, { buffer = bufnr })

  vim.keymap.set("n", "<cr>", goto_file_line_col, { buffer = bufnr })

  return self
end

function Job:open_win()
  local title = self.status

  local row_margin, col_margin = math.floor(vim.o.lines * 0.05), math.floor(vim.o.columns * 0.05)
  local win_config = {
    row = row_margin,
    col = col_margin,
    height = vim.o.lines - row_margin * 2 - 2,
    width = vim.o.columns - col_margin * 2 - 2,
    style = 'minimal',
    border = 'rounded',
    title = title,
    title_pos = 'center',
    relative = 'editor',
  }

  self.win = vim.api.nvim_open_win(self.buf, true, win_config)

  vim.api.nvim_create_autocmd("WinEnter", {
    group = augroup,
    callback = function()
      pcall(vim.api.nvim_win_close, self.win, true)
    end,
    once = true,
  })
end

---@class History
---@field jobs Job[]
---@field limit integer
local History = {}
History.__index = History

---@param limit integer?
---@return History
function History.new(limit)
  return setmetatable({ jobs = {}, limit = limit or 10, }, History)
end

---@param job Job
function History:add(job)
  self.jobs[self.limit] = nil
  table.insert(self.jobs, 1, job)
end

local M = {}
local history = History.new()

---@param cmd string
---@param stdin string?
local function run(cmd, stdin)
  local job = Job.start(cmd, stdin)
  history:add(job)
  job:open_win()
end

local function prompt_run()
  local row_margin, col_margin = math.floor(vim.o.lines * 0.2), math.floor(vim.o.columns * 0.2)

  -- Command Window

  local cmd_buf = vim.api.nvim_create_buf(false, true)
  local cmd_bufnr = vim.fn.bufnr(cmd_buf)

  local win_config = {
    row = row_margin,
    col = col_margin,
    height = math.floor((vim.o.lines - row_margin * 2 - 4) / 2),
    width = vim.o.columns - col_margin * 2 - 2,
    title = 'Command',
    title_pos = 'center',
    style = 'minimal',
    border = 'rounded',
    relative = 'editor',
  }

  local cmd_win = vim.api.nvim_open_win(cmd_buf, true, win_config)

  -- Stdin Window

  local stdin_buf = vim.api.nvim_create_buf(false, true)
  local stdin_bufnr = vim.fn.bufnr(stdin_buf)

  win_config.row = win_config.height + row_margin + 2
  win_config.height = vim.o.lines - win_config.height - row_margin * 2 - 4
  win_config.title = 'Stdin'

  local stdin_win = vim.api.nvim_open_win(stdin_buf, false, win_config)

  -- Options

  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = cmd_bufnr })
  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = stdin_bufnr })

  -- Mappings

  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = cmd_bufnr })
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = stdin_bufnr })

  local function switch_windows()
    local id = vim.fn.win_getid()
    if id == cmd_win then
      vim.api.nvim_set_current_win(stdin_win)
    elseif id == stdin_win then
      vim.api.nvim_set_current_win(cmd_win)
    end
  end

  vim.keymap.set("n", "<tab>", switch_windows, { buffer = cmd_bufnr })
  vim.keymap.set("n", "<tab>", switch_windows, { buffer = stdin_bufnr })

  local _run = function()
    local cmd = vim.fn.join(vim.api.nvim_buf_get_lines(cmd_buf, 0, -1, true), '\n')
    local stdin = vim.fn.join(vim.api.nvim_buf_get_lines(stdin_buf, 0, -1, true), '\n')
    run(cmd, stdin)
  end

  vim.keymap.set('n', '<cr>', _run,{ buffer = cmd_bufnr })
  vim.keymap.set('n', '<cr>', _run,{ buffer = stdin_bufnr })

  -- Autocmds

  local function close_windows()
    pcall(vim.api.nvim_win_close, cmd_win, true)
    pcall(vim.api.nvim_win_close, stdin_win, true)
  end

  local autocmds = Autocmds.new()

  local autocmd = vim.api.nvim_create_autocmd("WinEnter", {
    group = augroup,
    callback = function()
      local win_id = vim.fn.win_getid()
      if not (win_id == cmd_win or win_id == stdin_win) then
        close_windows()
      end
    end
  })
  autocmds:add(autocmd)

  autocmd = vim.api.nvim_create_autocmd("WinClosed", {
    group = augroup,
    pattern = string.format("%d,%d", cmd_win, stdin_win),
    callback = function()
      autocmds:delete()
      close_windows()
    end
  })
  autocmds:add(autocmd)
end

---@param cmd string?
---@param stdin string?
M.run = function(cmd, stdin)
  if cmd ~= nil then
    run(cmd, stdin)
  else
    prompt_run()
  end
end

M.show_last = function()
  if #history.jobs == 0 then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  history.jobs[1]:open_win()
end

M.rerun_last = function()
  if #history.jobs == 0 then
    vim.notify("[Exec] Empty history", vim.log.levels.WARN)
    return
  end

  run(history.jobs[1].cmd, history.jobs[1].stdin)
end

M.show_history = function()
  -- History Window

  local history_buf = vim.api.nvim_create_buf(false, true)
  local history_bufnr = vim.fn.bufnr(history_buf)

  local row_margin, col_margin = math.floor(vim.o.lines * 0.05), math.floor(vim.o.columns * 0.05)
  local win_config = {
    row = row_margin,
    col = col_margin,
    height = vim.o.lines - row_margin * 2 - 2,
    width = math.floor((vim.o.columns - col_margin * 2 - 4) * 0.4),
    title = 'History',
    title_pos = 'center',
    style = 'minimal',
    border = 'rounded',
    relative = 'editor',
  }

  local history_win = vim.api.nvim_open_win(history_buf, true, win_config)

  -- Command Window

  local cmd_buf = vim.api.nvim_create_buf(false, true)
  local cmd_bufnr = vim.fn.bufnr(cmd_buf)

  win_config.title = 'Command'
  win_config.col = col_margin + win_config.width + 2
  win_config.width = vim.o.columns - col_margin * 2 - win_config.width - 4
  win_config.height = math.floor((vim.o.lines - row_margin * 2 - 6) / 3)

  local cmd_win = vim.api.nvim_open_win(cmd_buf, false, win_config)

  -- Stdin Window

  local stdin_buf = vim.api.nvim_create_buf(false, true)
  local stdin_bufnr = vim.fn.bufnr(stdin_buf)

  win_config.title = 'Stdin'
  win_config.row = win_config.row + win_config.height + 2

  local stdin_win = vim.api.nvim_open_win(stdin_buf, false, win_config)

  -- Output Window

  local output_buf = vim.api.nvim_create_buf(false, true)
  local output_bufnr = vim.fn.bufnr(output_buf)

  win_config.title = 'Output'
  win_config.row = win_config.row + win_config.height + 2
  win_config.height = vim.o.lines - row_margin * 2 - win_config.height * 2 - 6

  local output_win = vim.api.nvim_open_win(output_buf, false, win_config)

  -- Update Windows

  ---@param idx integer
  local update_windows = function(idx)
    local job = history.jobs[idx]
    if job then
      vim.api.nvim_set_option_value('modifiable', true, { buf = cmd_bufnr })
      vim.api.nvim_buf_set_lines(cmd_buf, 0, -1, true, vim.fn.split(job.cmd, '\n'))
      vim.api.nvim_set_option_value('modifiable', false, { buf = cmd_bufnr })

      vim.api.nvim_set_option_value('modifiable', true, { buf = stdin_bufnr })
      vim.api.nvim_buf_set_lines(stdin_buf, 0, -1, true, vim.fn.split(job.stdin, '\n'))
      vim.api.nvim_set_option_value('modifiable', false, { buf = stdin_bufnr })

      vim.api.nvim_set_option_value('modifiable', true, { buf = output_bufnr })
      vim.api.nvim_buf_set_lines(output_buf, 0, -1, true, vim.api.nvim_buf_get_lines(history.jobs[idx].buf, 0, -1, true))
      vim.api.nvim_set_option_value('modifiable', false, { buf = output_bufnr })
    end
  end

  for i, job in ipairs(history.jobs) do
    vim.api.nvim_buf_set_lines(history_buf, i - 1, -1, true, { vim.fn.split(job.cmd, '\n')[1] })
  end

  update_windows(1)

  -- Options

  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = history_bufnr })
  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = cmd_bufnr })
  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = stdin_bufnr })
  vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = output_bufnr })

  vim.api.nvim_set_option_value('modifiable', false, { buf = history_bufnr })
  vim.api.nvim_set_option_value('modifiable', false, { buf = cmd_bufnr })
  vim.api.nvim_set_option_value('modifiable', false, { buf = stdin_bufnr })
  vim.api.nvim_set_option_value('modifiable', false, { buf = output_bufnr })

  -- Mappings

  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = history_bufnr })
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = cmd_bufnr })
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = stdin_bufnr })
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = output_bufnr })

  local next_win = { [history_win] = cmd_win, [cmd_win] = stdin_win, [stdin_win] = output_win, [output_win] = history_win, }
  local prev_win = { [history_win] = output_win, [output_win] = stdin_win, [stdin_win] = cmd_win, [cmd_win] = history_win, }
  local function switch_windows(fwd)
    local id = vim.fn.win_getid()
    if fwd then
      vim.api.nvim_set_current_win(next_win[id])
    else
      vim.api.nvim_set_current_win(prev_win[id])
    end
  end

  vim.keymap.set("n", "<tab>", function() switch_windows(true) end, { buffer = history_bufnr })
  vim.keymap.set("n", "<tab>", function() switch_windows(true) end, { buffer = cmd_bufnr })
  vim.keymap.set("n", "<tab>", function() switch_windows(true) end, { buffer = stdin_bufnr })
  vim.keymap.set("n", "<tab>", function() switch_windows(true) end, { buffer = output_bufnr })

  vim.keymap.set("n", "<S-tab>", function() switch_windows(false) end, { buffer = history_bufnr })
  vim.keymap.set("n", "<S-tab>", function() switch_windows(false) end, { buffer = cmd_bufnr })
  vim.keymap.set("n", "<S-tab>", function() switch_windows(false) end, { buffer = stdin_bufnr })
  vim.keymap.set("n", "<S-tab>", function() switch_windows(false) end, { buffer = output_bufnr })

  local _run = function()
    local cmd = vim.fn.join(vim.api.nvim_buf_get_lines(cmd_buf, 0, -1, true), '\n')
    local stdin = vim.fn.join(vim.api.nvim_buf_get_lines(stdin_buf, 0, -1, true), '\n')
    run(cmd, stdin)
  end

  vim.keymap.set('n', '<cr>', _run,{ buffer = history_bufnr })
  vim.keymap.set('n', '<cr>', _run,{ buffer = cmd_bufnr })
  vim.keymap.set('n', '<cr>', _run,{ buffer = stdin_bufnr })
  vim.keymap.set('n', '<cr>', _run,{ buffer = output_bufnr })

  -- Autocmds

  local function close_windows()
    pcall(vim.api.nvim_win_close, history_win, true)
    pcall(vim.api.nvim_win_close, cmd_win, true)
    pcall(vim.api.nvim_win_close, stdin_win, true)
    pcall(vim.api.nvim_win_close, output_win, true)
  end

  local autocmds = Autocmds.new()

  local autocmd = vim.api.nvim_create_autocmd("CursorMoved", {
    group = augroup,
    pattern = string.format("<buffer=%d>", history_bufnr),
    callback = function()
      update_windows(vim.fn.line('.'))
    end
  })
  autocmds:add(autocmd)

  autocmd = vim.api.nvim_create_autocmd("WinEnter", {
    group = augroup,
    callback = function()
      local win_id = vim.fn.win_getid()
      if not (win_id == history_win or win_id == cmd_win or win_id == stdin_win or win_id == output_win) then
        close_windows()
      end
    end
  })
  autocmds:add(autocmd)

  autocmd = vim.api.nvim_create_autocmd("WinClosed", {
    group = augroup,
    pattern = string.format("%d,%d,%d,%d", history_win, cmd_win, stdin_win, output_win),
    callback = function()
      autocmds:delete()
      close_windows()
    end
  })
  autocmds:add(autocmd)
end

M.setup = function()
end

return M
