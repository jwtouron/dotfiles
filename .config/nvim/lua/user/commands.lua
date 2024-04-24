vim.api.nvim_create_user_command(
  "ReadDate",
  "read !date '+\\%Y-\\%m-\\%d'",
  { desc = "Insert the current date as YYYY-MM-DD below the current line." }
)

local exec_window = nil

vim.api.nvim_create_user_command(
  "Exec",
  function(arg)
    if exec_window and vim.api.nvim_win_is_valid(exec_window) then return end

    -- Create the buffer
    local buf = vim.api.nvim_create_buf(false, true)
    local bufnr = vim.fn.bufnr(buf)
    vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })

    -- Calculate and set buffer contents
    local contents = vim.fn.execute(arg.args)
    contents = vim.split(contents, '\n')
    vim.api.nvim_buf_set_lines(buf, 0, -1, true, contents)

    vim.api.nvim_buf_set_option(bufnr, 'modifiable', false)

    -- Create the window
    local margin = 0.05
    local row = math.floor(vim.o.lines * margin)
    local col = math.floor(vim.o.columns * margin)
    local win_config = {
      relative = 'editor',
      width = math.floor(vim.o.columns * (1 - margin * 2)),
      height = math.floor(vim.o.lines * (1 - margin * 2)),
      row = row,
      col = col,
      style = 'minimal',
      border = 'rounded',
    }
    exec_window = vim.api.nvim_open_win(buf, true, win_config)
  end,
  {
    desc = "Open a floating window with the output of a command",
    nargs = 1,
  }
)
