local function goto_file_line_col()
  local patterns = {
    '^(.-):(%d+):(%d+):.*',                  -- Multiple
    '^(.-):(%d+):.*',                        -- Multiple
    '^--> (.-):(%d+):(%d+)',                 -- Rust
    '^  File "([^"]+)", line (%d+), in .*',  -- Python
  }

  local line = vim.fn.getline('.')

  for _, pattern in ipairs(patterns) do
    local file, line_num, col_num = string.match(line, pattern)
    line_num, col_num = tonumber(line_num), tonumber(col_num)
    if file and vim.fn.filereadable(file) == 1 and line_num then
      col_num = col_num or 1
      vim.cmd("silent close | silent edit " .. vim.fn.fnameescape(file))
      vim.fn.cursor(line_num, col_num)
      return
    end
  end
end

return {
  "akinsho/toggleterm.nvim",
  version = "*",
  cmd = "ToggleTerm",
  keys = {
    { "<leader>t1", "<cmd>1ToggleTerm<cr>", },
    { "<leader>t2", "<cmd>2ToggleTerm<cr>", },
    { "<leader>t3", "<cmd>3ToggleTerm<cr>", },
    { "<leader>t4", "<cmd>4ToggleTerm<cr>", },
    { "<leader>t5", "<cmd>5ToggleTerm<cr>", },
    { "<leader>t6", "<cmd>6ToggleTerm<cr>", },
  },
  opts = {
    on_create = function(term)
      vim.keymap.set("n", "<cr>", goto_file_line_col, { buffer = term.bufnr })

      local resize = term.resize
      term.resize = function(t, s)
        if t.direction == "vertical" then
          s = math.floor(vim.o.columns * 0.4)
        end
        resize(t, s)
      end
    end,
    on_open = function(term)
      if term.direction == "vertical" then
        term:resize()
      end
    end,
  },
}
