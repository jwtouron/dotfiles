local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

function MyFindFunc(cmdarg)
  local cmd = ''

  if vim.fn.executable('fd') == 1 then
    cmd = "fd --type file --unrestricted --ignore-case --exclude '.git'"
    for _, path in ipairs(vim.opt.path:get()) do
      if path ~= "." then
        if path == "" then
          cmd = cmd .. ' --search-path .'
        else
          cmd = cmd .. ' --search-path ' .. path
        end
      end
    end
  elseif vim.fn.executable('rg') == 1 then
  else
  end

  cmd = cmd .. ' | fzf -f ' .. vim.fn.shellescape(cmdarg)

  return vim.fn.systemlist(cmd)
end

vim.opt.findfunc = 'v:lua.MyFindFunc'

vim.keymap.set('n', '<leader><space>', ":find ")

vim.api.nvim_create_user_command(
  "Buffers",
  function(arg)
    vim.cmd("e " .. arg.args)
  end,
  {
    nargs = 1,
    complete = function(arglead, cmdline, cursorpos)
      local bufnames = {}
      local curbufnr = vim.api.nvim_get_current_buf()
      for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
        if bufnr ~= curbufnr then
          local ok, bufname = pcall(vim.api.nvim_buf_get_name, bufnr)
          if ok and vim.fn.buflisted(bufnr) == 1 and vim.fn.bufloaded(bufnr) == 1 and bufname ~= "" then
            table.insert(bufnames, bufname)
          end
        end
      end
      return vim.fn.systemlist(string.format("cat << 'EOF' | fzf -f %s\n%s\nEOF\n", vim.fn.shellescape(arglead), vim.fn.join(bufnames, '\n')))
    end
  }
)

vim.keymap.set('n', '<leader>,', ":Buffers ")

vim.api.nvim_create_user_command(
  "History",
  function(arg)
    vim.cmd(arg.args)
  end,
  {
    nargs = 1,
    complete = function(arglead, cmdline, cursorpos)
      local awkprog = '$1 != "#" { if ($1 == ">") { $2 = "" }; $1 = ""; sub(/^ */, "", $0); print $0 }'
      return vim.fn.systemlist(string.format("cat << 'EOF' | awk '%s' | sort -r | fzf -f %s\n%s\nEOF\n", awkprog, vim.fn.shellescape(arglead), vim.fn.execute('history')))
    end
  }
)

vim.keymap.set('n', '<leader>ch', ':History ')

vim.api.nvim_create_autocmd("CmdlineChanged", {
  group = augroup,
  pattern = ':',
  callback = (function()
    local current_id = 0
    return function()
      current_id = current_id + 1
      local id = current_id
      vim.defer_fn(
        function()
          if id == current_id and vim.fn.pumvisible() == 0 and not vim.fn.getcmdline():match('^%s*%S*$') then
            vim.api.nvim_input("<tab>")
          end
        end,
        300
      )
    end
  end)(),
})
