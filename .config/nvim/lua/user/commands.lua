for _, cmd in ipairs({ "Cdf", "CDF" }) do
  vim.api.nvim_create_user_command(
    cmd,
    [[execute 'cd' expand('%:p:h')]],
    { desc = "cd to the directory of the current file" }
  )
end

vim.api.nvim_create_user_command(
  "ReadDate",
  "read !date '+\\%Y-\\%m-\\%d'",
  { desc = "Insert the current date as YYYY-MM-DD below the current line." }
)

vim.api.nvim_create_user_command(
  "Todos",
  function(arg)
    local dir = arg.args ~= '' and arg.args or '.'
    local grepprg = vim.opt.grepprg:get()  -- save grepprg
    local regex = '(FIXME\\|HACK\\|NOTE\\|TODO) *(\\([^)]*\\))? *:'
    if vim.fn.executable("rg") == 1 then
      vim.opt.grepprg = "rg --vimgrep '" .. regex .. "' " .. dir
    else
      vim.opt.grepprg = "grep -rnE '" .. regex .. "' " .. dir
    end
    local bang = arg.bang and '!' or ''
    pcall(function() vim.cmd("grep" .. bang) end)
    vim.opt.grepprg = grepprg  -- restore grepprg
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
