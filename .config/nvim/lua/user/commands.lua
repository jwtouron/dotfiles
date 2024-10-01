-- vim: set foldenable foldmethod=marker:

-- {{{1 Bd, BD, Bw, BW: Wipe and delete buffer

for _, cmd in ipairs({ "Bd", "BD" }) do
  vim.api.nvim_create_user_command(cmd, [[b#|bd#]], {})
end

for _, cmd in ipairs({ "Bw", "BW" }) do
  vim.api.nvim_create_user_command(cmd, [[b#|bw#]], {})
end

-- {{{1 Cdf, CDF: Change to directory of current file

for _, cmd in ipairs({ "Cdf", "CDF" }) do
  vim.api.nvim_create_user_command(
    cmd,
    [[execute 'cd' expand('%:p:h')]],
    { desc = "cd to the directory of the current file" }
  )
end

-- {{{1 E: Execute a command and put the output in a new buffer.

vim.api.nvim_create_user_command(
  "E",
  (function()
    local last_command = ""
    local buffer = nil

    return function(arg)
      if arg.args == "" and last_command == "" then
        vim.api.nvim_err_writeln("Argument required.")
        return
      end

      if arg.bang then
        pcall(function(x) vim.cmd(x) end, "silent write")  -- pcall in case buffer isn't writable... just ignore
      end

      local command = nil

      if arg.args == "" then
        command = last_command
      else
        command = arg.args
      end

      if buffer and vim.api.nvim_buf_is_valid(buffer) then
        vim.api.nvim_buf_delete(buffer, { force = true })
        buffer = nil
      end

      buffer = vim.api.nvim_create_buf(false, true)
      local bufnr = vim.fn.bufnr(buffer)
      vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = bufnr })

      vim.cmd("b " .. bufnr)
      vim.cmd("silent 0file | silent keepalt noautocmd file exec:///" .. command)
      vim.keymap.set('n', 'q', '<cmd>b# | bw #<cr>', { buffer = bufnr })

      local ok, output = pcall(vim.fn.execute, command)
      if ok then
        vim.api.nvim_buf_set_lines(buffer, 0, -1, true, vim.split(output, '\n'))
        vim.api.nvim_set_option_value('modifiable', false, { buf = bufnr })
        last_command = command
      end
    end
  end)(),
  {
    bang = true,
    desc = "Execute a command and put the output in a new buffer.",
    nargs = "*",
  }
)

-- {{{1  ReadDate

vim.api.nvim_create_user_command(
  "ReadDate",
  function(arg)
    local command = "read !date"
    if arg.args ~= "" then
      local begin, _, sign, amt, unit = string.find(arg.args, '^([-+]?)(%d+)([mdy])$')
      if not begin then
        vim.api.nvim_err_writeln("[ReadDate] Invalid argument: " .. arg.args)
        return
      end
      local unit = ({ d = 'day', m = 'month', y = 'year' })[unit]
      command = command .. " -d '" .. amt .. " " .. unit
      if sign == '-' then
        command = command .. " ago"
      end
      command = command .. "'"
    end
    command = command .. " '+\\%Y-\\%m-\\%d'"
    vim.cmd(command)
  end,
  {
    desc = "Insert the current date as YYYY-MM-DD below the current line.",
    nargs = '?',
  }
)

-- {{{1 R, RD, RL: Rerun Commands

local rerun_command = { saved_commands = {} }

vim.api.nvim_create_user_command(
  "R",
  function(arg)
    local pattern = nil
    local index = nil
    local command = nil

    if arg.args ~= "" then
      pattern = arg.args
    end

    if arg.range == 1 then
      index = arg.line1
    end

    if pattern then
      -- Search for pattern in history
      local history = vim.fn.split(vim.fn.execute('silent history'), '\n')
      for i = #history, 1, -1 do
        if history[i]:find(pattern) and not history[i]:find('%d*R .*' .. pattern) then
          command = history[i]:gsub("^>?%s*%d+%s*", "")
          break
        end
      end

      if not command then
        vim.api.nvim_err_writeln("Could not find command in history: " .. pattern)
        return
      end

      if index then
        rerun_command.saved_commands[index] = command
      end
    elseif index then
      -- No command given, but have index... execute command saved at index
      if rerun_command.saved_commands[index] then
        command = rerun_command.saved_commands[index]
      else
        vim.api.nvim_err_writeln("No saved command at given index: " .. index)
        return
      end
    else
      -- Neither pattern nor index, find first saved command, if exists
      for _, cmd in ipairs(rerun_command.saved_commands) do
        if cmd then
          command = cmd
          break
        end
      end

      if not command then
        vim.api.nvim_err_writeln("No saved commands!")
        return
      end
    end

    vim.cmd(command)
  end,
  {
    desc = "Rerun a command",
    nargs = '?',
    count = 1,
    bar = true,
  }
)

vim.api.nvim_create_user_command(
  "RD",
  function(arg)
    local indices = {}

    for _, arg in ipairs(arg.fargs) do
      if not arg:find("^%d(-%d)?$") then
        vim.api.nvim_err_writeln("Arguments for RD must be in the form: RD 1-2 3")
        return
      end

      local splits = vim.fn.split(arg, "-")
      for i, num in ipairs(splits) do splits[i] = tonumber(num) end

      if #splits == 1 then
        table.insert(indices, splits[1])
      else
        for i = splits[1], splits[2] do
          table.insert(indices, i)
        end
      end
    end

    for _, index in ipairs(indices) do
      rerun_command.saved_commands[index] = nil
    end
  end,
  {
    desc = "Delete a saved rerun command",
    nargs = "+",
    bar = true,
  }
)

vim.api.nvim_create_user_command(
  "RL",
  function()
    for i, command in ipairs(rerun_command.saved_commands) do
      if command then
        print(i, command)
      end
    end
  end,
  {
    desc = "List saved rerun commands",
    bar = true,
  }
)

-- {{{1 Todos

vim.api.nvim_create_user_command(
  "Todos",
  function(arg)
    local dir = arg.args ~= '' and arg.args or '.'
    local grepprg = vim.opt.grepprg:get()  -- save grepprg
    local grepformat = vim.opt.grepformat:get()  -- save grepformat
    local regex = '(FIXME\\|HACK\\|NOTE\\|TODO) *(\\([^)]*\\))? *:'
    if vim.fn.executable("rg") == 1 then
      vim.opt.grepprg = "rg --vimgrep '" .. regex .. "' " .. dir
    else
      vim.opt.grepprg = "grep -HInrE '" .. regex .. "' " .. dir
      vim.opt.grepformat = "%f:%l:%m"
    end
    local bang = arg.bang and '!' or ''
    pcall(function() vim.cmd("grep" .. bang) end)
    vim.opt.grepprg = grepprg  -- restore grepprg
    vim.opt.grepformat = grepformat  -- restore grepformat
  end,
  {
    desc = "Find all FIXMEs, HACKs, NOTEs, and TODOs",
    nargs = '?',
    bang = true,
  }
)

-- {{{1 Tui, TUI: Run a TUI application using :term

vim.api.nvim_create_autocmd("TermClose", {
  group = vim.api.nvim_create_augroup("user-tui", { clear = true }),
  command = "b#|bw#",
  -- callback = function()
  --   require('mini.bufremove').wipeout(0)
  -- end
})
--
for _, name in ipairs({ "Tui", "TUI" }) do
  vim.api.nvim_create_user_command(
    name,
    function(arg)
      vim.cmd(
        "enew | exec 'term " .. arg.args .. "' | setl nonumber norelativenumber | startinsert"
      )
    end,
    {
      nargs = 1,
    }
  )
end
