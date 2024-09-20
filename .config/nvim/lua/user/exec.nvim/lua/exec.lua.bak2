local run = {}
local history = { commands = {} }

local function buffer_is_valid(buffer)
  return buffer ~= nil and vim.api.nvim_buf_is_valid(buffer)
end

local function window_is_valid(window)
  return window ~= nil and vim.api.nvim_win_is_valid(window)
end

local function window_close(window)
  vim.api.nvim_win_close(window[1], true)
  window[1] = nil
end

local function window_close_if_valid(window)
  if window_is_valid(window[1]) then
    window_close(window)
  end
end

local function jobstop(jobid)
  if jobid[1] then
    vim.fn.jobstop(jobid[1])
    jobid[1] = nil
  end
end

local function open_window(buffer, title)
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
  return vim.api.nvim_open_win(buffer, true, win_config)
end

local function start_job(command, buffer, stdin)
  local bufnr = vim.fn.bufnr(buffer)

  local function append_line(line)
    vim.api.nvim_set_option_value('modifiable', true, { buf = bufnr })
    vim.api.nvim_buf_set_lines(buffer, -1, -1, true, { line })
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
      for i = 2, #data - 1 do
        append_line(data[i])
      end
    end,
    on_exit = function()
      if run.title == "Running..." then
        run.title = 'Done'
        vim.api.nvim_win_set_config(0, { title = run.title, title_pos = 'center', })
      end
    end,
    stderr_buffered = false,
    stdout_buffered = false,
  }
  opts.on_stderr = opts.on_stdout
  local jobid = vim.fn.jobstart(command, opts)
  if stdin then
    vim.fn.chansend(jobid, stdin)
    vim.fn.chanclose(jobid, 'stdin')
  end
  return jobid
end

local M = {}

M.run = function(command, stdin)
  if command:find('^ *$') then
    return
  end

  window_close_if_valid({ history.window })

  window_close_if_valid({ run.window })

  if run.jobid then
    jobstop({ run.jobid })
  end

  if buffer_is_valid(run.buffer) then
    vim.api.nvim_buf_delete(run.buffer, { force = true })
    run.buffer = nil
  end

  run.buffer = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(run.buffer)
  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })

  run.title = 'Running...'
  run.jobid = start_job(command, run.buffer, stdin)
  run.window = open_window(run.buffer, run.title)

  vim.keymap.set('n', '<C-c>', function()
    jobstop({ run.jobid })
    run.title = 'Aborted.'
    vim.api.nvim_win_set_config(0, { title = run.title, title_pos = 'center', })
  end,
  { buffer = bufnr })

  table.insert(history.commands, 1, { command, stdin })
  for i = 2, #history.commands do
    if history.commands[i][1] == command then
      table.remove(history.commands, i)
      break
    end
  end
end

M.open_history = function()
  if window_is_valid(history.window) then
    return
  end

  window_close_if_valid({ run.window })

  if not buffer_is_valid(history.buffer) then
    history.buffer = vim.api.nvim_create_buf(false, true)
    local bufnr = vim.fn.bufnr(history.buffer)

    local function update_commands_from_buffer(first, stdin)
      local new_commands = {}
      if first then table.insert(new_commands, { first, stdin }) end
      for _, c1 in ipairs(vim.api.nvim_buf_get_lines(history.buffer, 0, -1, false)) do
        if c1 ~= first and not c1:find('^ *$') then
          for _, c2 in ipairs(history.commands) do
            if c1 == c2[1] then
              table.insert(history.commands, c2)
              break
            end
          end
        end
      end
      history.commands = new_commands
    end

    vim.keymap.set('n', 'q', function()
      update_commands_from_buffer()
      window_close({ history.window })
    end, { buffer = bufnr })

    vim.keymap.set('n', '<cr>', function()
      local command = vim.fn.getline('.')
      update_commands_from_buffer(command)
      window_close({ history.window })
      M.run(command)
    end, { buffer = bufnr })
  end

  local replacement = {}
  for _, cmd in ipairs(history.commands) do
    table.insert(replacement, cmd[1])
  end
  vim.api.nvim_buf_set_lines(history.buffer, 0, -1, true, replacement)

  history.window = open_window(history.buffer, 'History')
end

M.rerun = function()
  if history.commands[1] then
    M.run(history.commands[1][1], history.commands[1][2])
  else
    vim.api.nvim_err_writeln('[Exec] Empty history!')
  end
end

M.last_command = function()
  return history.commands[1]
end

M.toggle_output = function()
  if not buffer_is_valid(run.buffer) then
    vim.api.nvim_err_writeln('[Exec] Cannot display last output: Either a command has never been run or the the buffer has been deleted.')
    return
  end
  if window_is_valid(run.window) then
    window_close({ run.window })
  else
    if window_is_valid(history.window) then
      window_close({ history.window })
    end
    open_window(run.buffer, run.title)
  end
end

M.setup = function()
  vim.api.nvim_create_user_command(
    'Exec',
    function(arg)
      local stdin = nil
      if arg.range ~= 0 then
        stdin = vim.fn.join(vim.api.nvim_buf_get_lines(0, arg.line1 - 1, arg.line2, true), '\n')
      end
      M.run(arg.args, stdin)
    end,
    { nargs = 1, range = true, }
  )
end

return M
