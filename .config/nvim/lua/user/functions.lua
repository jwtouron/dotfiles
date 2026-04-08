local M = {}

M.SetFindFunc = function(...)
  local dirs = {...}
  local dirstr = '.'
  for _, dir in ipairs(dirs) do
    if dir ~= '.' then
      dirstr = dirstr .. ' ' .. vim.fn.fnamemodify(dir, ':p:.:S')
    end
  end

  local cmd = 'find ' .. dirstr .. " -path '*/.git' -prune -o -type f -print"
  if vim.fn.executable('fd') == 1 then
    cmd = "fd --type file --unrestricted --exclude '.git' '' " .. dirstr
  end

  local has_fzf = vim.fn.executable('fzf') == 1

  _G.MyFindFunc = function(cmdarg)
    vim.opt_local.busy = 1
    vim.cmd.redrawstatus()

    local cmd = cmd .. (has_fzf and (" | fzf --filter " .. vim.fn.shellescape(cmdarg)) or '')
    local result = vim.fn.systemlist(cmd)

    if not has_fzf and cmdarg ~= '' then
      result = vim.fn.matchfuzzy(result, cmdarg)
    end

    vim.opt_local.busy = 0

    return result
  end

  vim.opt.findfunc = 'v:lua.MyFindFunc'
end

M.setup = function()
  for f in pairs(M) do
    if f:match("^[A-Z]") then
      _G[f] = M[f]
    end
  end
end

return M
