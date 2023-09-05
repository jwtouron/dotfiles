vim.api.nvim_create_user_command(
  "ReadDate",
  "read !date '+\\%Y-\\%m-\\%d'",
  { desc = "Insert the current date as YYYY-MM-DD below the current line." }
)

for _, cmd in ipairs({ "Cdf", "CDF" }) do
  vim.api.nvim_create_user_command(
    cmd,
    [[execute 'cd' expand('%:p:h')]],
    { desc = "cd to the directory of the current file" }
  )
end

-- https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3

vim.cmd [[

function! Grep(...)
  return system(join([&grepprg] + [expandcmd(join(a:000, ' '))], ' '))
endfunction

command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr Grep(<f-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr Grep(<f-args>)

cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'

" augroup quickfix
"   autocmd!
"   autocmd QuickFixCmdPost cgetexpr cwindow
"   autocmd QuickFixCmdPost lgetexpr lwindow
" augroup END

]]

-- Random Color Schemes

local function random_color_scheme()
  local color_schemes = vim.fn.getcompletion("", "color")
  if color_schemes then
    local idx = math.random(1, #color_schemes)
    local color_scheme = color_schemes[idx]
    vim.cmd.colorscheme(color_scheme)
    print("Color scheme set: " .. color_scheme)
  end
end

vim.api.nvim_create_user_command("RandomColorScheme", random_color_scheme, {})
