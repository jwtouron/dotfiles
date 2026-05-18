local M = {}

M.SetFindFunc = function(...)
  local default_dirs = {...}
  local default_dirstr = vim.fn.join(
    vim.fn.map(default_dirs, "v:val == '.' ? '.' : fnamemodify(v:val, ':p:.:S')"),
    " "
  )

  function MyFindFunc(cmdarg)
    cmdarg = vim.fn.fnamemodify(cmdarg, ":p:.")
    if cmdarg:sub(1, 1) == "/" then
      local segments = vim.fn.split(cmdarg, "/", true)
      local paths = { "/" }
      for i = 2, #segments - 1 do
        local segment = vim.fn.escape(segments[i], "*")
        local new_paths = {}
        for _, path in ipairs(paths) do
          new_paths = vim.fn.extend(
            new_paths,
            vim.fn.systemlist(
              string.format(
                "fd --unrestricted --max-depth 1 --type dir --exclude '.git' --glob %s %s",
                vim.fn.shellescape("*"..segment.."*"),
                vim.fn.shellescape(path)
              )
            )
          )
        end
        paths = new_paths
      end
      local result = {}
      for _, path in ipairs(paths) do
        result = vim.fn.extend(
          result,
          vim.fn.systemlist(
            string.format("fd --unrestricted --max-depth 1 --exclude '.git' --glob '*' %s", vim.fn.shellescape(path))
          )
        )
      end
      local filter = segments[#segments]
      if filter ~= "" then
        result = vim.fn.matchfuzzy(result, filter)
      end
      return result
    else
      local cmd = string.format(
        "fd --unrestricted --exclude '.git' . %s | fzf --filter %s",
        default_dirstr, vim.fn.shellescape(cmdarg)
      )
      return vim.fn.systemlist(cmd)
    end
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
