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
  keys = function()
    local diffcmd = "If you propose edits, reply ONLY with a unified diff (udiff) with correct paths (---/+++), no extra commentary."
    local Terminal = require('toggleterm.terminal').Terminal
    local t = Terminal:new {
      cmd = "codex --ask-for-approval on-request --search " .. vim.fn.shellescape(diffcmd) .. " || read",
      direction = 'float',
    }

    local function send(first_line, last_line)
      if first_line and last_line and first_line > last_line then
        first_line, last_line = last_line, first_line
      end
      local filename = vim.api.nvim_buf_get_name(0)
      if filename ~= "" and vim.fn.filereadable(filename) ~= 0 then
        filename = vim.fn.fnamemodify(filename, ':.')
        t:open()
        t:send(string.format("%s%s%s", filename, first_line and ":" .. first_line or "", last_line and "-" .. last_line or ""))
      else
        vim.notify("No readable file associated with buffer.", vim.log.levels.INFO)
      end
    end

    local function send_visual()
      local pos1 = vim.fn.getpos('.')
      local pos2 = vim.fn.getpos('v')
      local lnum1 = pos1[2]
      local lnum2 = pos2[2]
      if not (lnum1 and lnum1 > 0 and lnum2 and lnum2 > 0) then
        error(string.format("invalid line numbers: %s %s", lnum1, lnum2))
      end
      send(lnum1, lnum2)
    end

    local keys = {
      { "<leader>t1", "<cmd>1ToggleTerm<cr>", desc = "ToggleTerm: toggle terminal 1" },
      { "<leader>t2", "<cmd>2ToggleTerm<cr>", desc = "ToggleTerm: toggle terminal 2" },
      { "<leader>t3", "<cmd>3ToggleTerm<cr>", desc = "ToggleTerm: toggle terminal 3" },
      { "<leader>t4", "<cmd>4ToggleTerm<cr>", desc = "ToggleTerm: toggle terminal 4" },
      { "<leader>t5", "<cmd>5ToggleTerm<cr>", desc = "ToggleTerm: toggle terminal 5" },
      { "<leader>t6", "<cmd>6ToggleTerm<cr>", desc = "ToggleTerm: toggle terminal 6" },
      { "<leader>tcc", function() t:toggle() end, desc = "ToggleTerm: toggle codex terminal" },
      { "<leader>tcf", function() send() end, desc = "ToggleTerm: send file name to codex" },
      { "<leader>tcl", function() send(vim.fn.line('.')) end, desc = "ToggleTerm: send line to codex" },
      { "<leader>tcl", send_visual, mode = 'x', desc = "ToggleTerm: send line(s) to codex (visual)" },
    }
    return keys
  end,
  opts = {
    on_create = function(term)
      vim.keymap.set("n", "<cr>", goto_file_line_col, { buffer = term.bufnr })

      local resize = term.resize
      term.resize = function(t, s)
        if t.direction == "vertical" then
          s = math.floor(vim.o.columns * 0.4)
        elseif t.direction == 'horizontal' then
          s = math.floor(vim.o.lines * 0.4)
        end
        resize(t, s)
      end
    end,
    on_open = function(term)
      if term.direction == "vertical" or term.direction == "horizontal" then
        term:resize()
      end
    end,
  },
}
