function MySetColor(color)
  local fn = vim.fn
  local env_path = fn.fnamemodify(fn.environ()['MYVIMRC'], ':p:h') .. '/env.json'

  local status, env = pcall(function()
    return fn.json_decode(fn.readfile(env_path, "B"))
  end)
  if not status then env = {} end

  if color then
    vim.cmd.colorscheme(color)
    env["colorscheme"] = color
    fn.writefile({fn.json_encode(env)}, env_path)
  elseif env["colorscheme"] then
    vim.cmd.colorscheme(env["colorscheme"])
  end

  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

  vim.cmd [[
  highlight MatchParen term=underline cterm=underline gui=underline ctermbg=NONE guibg=NONE
  ]]
end

MySetColor()

vim.cmd [[
command! -nargs=1 -complete=color Colorscheme lua MySetColor(<f-args>)
command! -nargs=1 -complete=color Colo lua MySetColor(<f-args>)
]]
