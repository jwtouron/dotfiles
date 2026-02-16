-- vim: set foldenable foldmethod=marker:

-- {{{1 CDC = Change to Directory of Current file

vim.api.nvim_create_user_command(
  'CDC',
  function() vim.cmd("cd %:p:h | pwd") end,
  { desc = "cd to the directory of the current file" }
)

-- {{{1 E: Execute a command and put the output in a new buffer.

vim.api.nvim_create_user_command(
  "E",
  (function()
    local last_command = ""
    local buffer = nil

    local goto_file_line_col = function()
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
          vim.cmd("silent edit " .. vim.fn.fnameescape(file))
          vim.fn.cursor(line_num, col_num)
          return
        end
      end
    end

    return function(arg)
      if arg.args == "" and last_command == "" then
        error("Argument required.")
        return
      end

      if arg.bang or vim.o.autowrite then
        pcall(function(x) vim.cmd(x) end, "silent write")  -- pcall in case buffer isn't writable... just ignore
      end

      local command = nil

      if arg.args == "" then
        command = last_command
      else
        command = arg.args
      end

      local output = vim.fn.execute(command)

      if buffer and vim.api.nvim_buf_is_valid(buffer) then
        vim.api.nvim_buf_delete(buffer, { force = true })
      end

      buffer = vim.api.nvim_create_buf(true, true)
      local bufnr = vim.fn.bufnr(buffer)

      vim.keymap.set('n', 'q', '<cmd>b#<cr>', { buffer = bufnr })
      vim.keymap.set('n', '<cr>', goto_file_line_col, { buffer = bufnr })

      vim.api.nvim_buf_set_lines(buffer, 0, -1, true, vim.split(output, '\n'))
      vim.api.nvim_set_option_value('modifiable', false, { buf = bufnr })

      vim.cmd("b " .. bufnr)
      vim.cmd("silent 0file | silent keepalt noautocmd file exec://" .. command)

      last_command = command
    end
  end)(),
  {
    bang = true,
    desc = "Execute a command and put the output in a new buffer.",
    nargs = "*",
  }
)

-- {{{1 Oldfiles

vim.api.nvim_create_user_command(
  "Oldfiles",
  function(arg)
    vim.cmd("edit " .. arg.args)
  end,
  {
    nargs = 1,
    complete = function(arglead)
      return vim.fn.matchfuzzy(vim.v.oldfiles, arglead)
    end,
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
