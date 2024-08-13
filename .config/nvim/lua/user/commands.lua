for _, cmd in ipairs({ "Cdf", "CDF" }) do
  vim.api.nvim_create_user_command(
    cmd,
    [[execute 'cd' expand('%:p:h')]],
    { desc = "cd to the directory of the current file" }
  )
end

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

vim.api.nvim_create_autocmd("TermClose", {
  group = vim.api.nvim_create_augroup("user-tui", { clear = true }),
  callback = function()
    require('mini.bufremove').wipeout(0)
  end
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
