local function win_config()
  local col_margin = math.floor(vim.o.columns * 0.1)
  local row_margin = math.floor(vim.o.lines * 0.1)
  return {
    row = row_margin,
    col = col_margin,
    height = vim.o.lines - row_margin * 2,
    width = vim.o.columns - col_margin * 2,
    style = 'minimal',
    border = 'none',
    relative = 'editor',
  }
end

local function fzf(input, fzf_opts, handlers)
  local resources = { cleaned = false }
  resources.cleanup = function(r)
    if r.cleaned then return end
    r.cleaned = true

    pcall(vim.fn.jobstop, r.job_id)
    pcall(vim.api.nvim_win_close, r.win, true)
    pcall(vim.api.nvim_buf_delete, r.buf, { force = true })
    pcall(vim.api.nvim_del_autocmd, r.vim_resized_autocmd)
    pcall(vim.fn.delete, r.output_file)
    pcall(vim.fn.delete, r.input_file)
  end

  fzf_opts = fzf_opts or ""

  resources.buf = vim.api.nvim_create_buf(false, true)
  resources.win = vim.api.nvim_open_win(resources.buf, true, win_config())
  vim.cmd "startinsert"

  resources.output_file = vim.fn.tempname()

  local expect_str = string.format(
    "--expect %s",
    vim.fn.shellescape(vim.fn.join(vim.fn.keys(handlers), ","))
  )
  local cmd = string.format(
    "fzf --print-query %s %s > %s",
    fzf_opts,
    expect_str,
    vim.fn.shellescape(resources.output_file)
  )

  if type(input) == 'table' then
    resources.input_file = vim.fn.tempname()
    vim.fn.writefile(input, resources.input_file)
    cmd = string.format("%s < %s", cmd, vim.fn.shellescape(resources.input_file))
  else
    cmd = string.format("%s | %s", input, cmd)
  end

  resources.job_id = vim.fn.jobstart(cmd, {
    term = true,
    on_exit = function(_, exit_code)
      local ok, result = pcall(function()
        if exit_code == 0 then
          local output = vim.fn.readfile(resources.output_file)
          local complete_key = output[2]
          if complete_key == "" then complete_key = "enter" end
          table.remove(output, 1)
          table.remove(output, 1)
          return function()
            handlers[complete_key](output)
          end
        else
          return function() end
        end
      end)

      resources:cleanup()

      if ok then
        result()
      else
        error(result)
      end
    end,
  })

  resources.vim_resized_autocmd = vim.api.nvim_create_autocmd("VimResized", {
    callback = function()
      if resources.win and vim.api.nvim_win_is_valid(resources.win) then
        vim.api.nvim_win_set_config(resources.win, win_config())
      end
    end,
  })

  vim.api.nvim_create_autocmd("WinLeave", {
    callback = function()
      resources:cleanup()
    end,
    once = true,
  })
end

local default_dirs = {}

local seen = {}
for _, path in ipairs(vim.opt.path:get()) do
  if path == "." then
    -- skip
  elseif path == "" then
    if not seen["."] then
      table.insert(default_dirs, ".")
      seen["."] = true
    end
  else
    local parts = vim.fn.split(path, "/", true)
    local dir = {}
    for _, part in ipairs(parts) do
      if part:find("^%*") or part:find("[^\\]%*") then
        break
      else
        table.insert(dir, part)
      end
    end
    local dir = vim.fn.join(dir, "/")
    if dir == "" then dir = "." end
    if not seen[dir] then
      table.insert(default_dirs, dir)
      seen[dir] = true
    end
  end
end

function FZFFiles(dirs)
  if not dirs then dirs = default_dirs end
  fzf(
    string.format(
      "fd --unrestricted --exclude '.git' --type file . %s",
      vim.fn.join(vim.fn.map(dirs, "v:val == '.' ? '.' : fnamemodify(v:val, ':p:.:S')"), " ")
    ),
    "--style=full --border --reverse --multi --border-label=Files",
    {
      enter = function(lines)
        for _, line in ipairs(lines) do
          if line ~= "" then
            vim.api.nvim_cmd({ cmd = "edit", args = { line } }, {})
          end
        end
      end
    }
  )
end

vim.api.nvim_create_user_command(
  "FzfFiles",
  function(arg)
    local dirs = nil
    if #arg.fargs > 0 then dirs = arg.fargs end
    FZFFiles(dirs)
  end,
  {
    nargs = "?",
    complete = "dir",
  }
)

vim.keymap.set("n", "<leader><space>", FZFFiles)
vim.keymap.set("n", "<leader>ff", ":FzfFiles ")

local function oldfiles()
  fzf(
    vim.v.oldfiles,
    "--style=full --border --reverse --border-label='Old Files'",
    {
      enter = function(lines)
        if lines[1] ~= "" then
          vim.api.nvim_cmd({ cmd = "edit", args = { lines[1] } }, {})
        end
      end
    }
  )
end

vim.keymap.set("n", "<leader>fo", oldfiles)
