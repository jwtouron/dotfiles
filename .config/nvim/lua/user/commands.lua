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

      local ok, output = pcall(vim.fn.execute, command)
      if ok then
        if buffer and vim.api.nvim_buf_is_valid(buffer) then
          vim.api.nvim_buf_delete(buffer, { force = true })
          buffer = nil
        end

        buffer = vim.api.nvim_create_buf(false, true)
        local bufnr = vim.fn.bufnr(buffer)
        vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = bufnr })
        vim.keymap.set('n', 'q', '<cmd>b#<cr>', { buffer = bufnr })

        vim.cmd("b " .. bufnr)
        vim.cmd("silent 0file | silent keepalt noautocmd file exec:///" .. command)

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
