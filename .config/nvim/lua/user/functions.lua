local M = {}

M.SetFindFunc = function(...)
  local default_dirs = {...}

  local run_fd = function(pattern, paths, args)
    args = args or ""
    return vim.fn.systemlist(
     string.format(
       "fd --unrestricted --exclude '.git' --glob %s %s %s",
       args,
       vim.fn.shellescape(pattern),
       vim.fn.join(
         vim.fn.map(paths, "v:val == '.' ? '.' : fnamemodify(v:val, ':p:.:S')"),
         " "
       )
      )
    )
  end

  function MyFindFunc(cmdarg)
    cmdarg = vim.fn.fnamemodify(cmdarg, ":p:.")
    local filter = cmdarg
    local result = {}

    if cmdarg:sub(1, 1) == "/" then
      local segments = vim.fn.split(cmdarg, "/", true)
      local paths = { "/" }
      for i = 2, #segments - 1 do
        local segment = vim.fn.escape(segments[i], "*")
        paths = run_fd("*"..segment.."*", paths, "--max-depth 1 --type dir")
      end
      filter = segments[#segments]
      result = run_fd('*', paths)
    else
      result = run_fd('*', default_dirs)
    end

    if filter ~= "" then
      result = vim.fn.matchfuzzy(result, filter)
    end
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
