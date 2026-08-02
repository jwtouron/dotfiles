local function goto_file_line_col()
  local line = vim.fn.getline('.')

  -- if that fails, try custom formats
  local patterns = {
    '^(.-):(%d+):(%d+):.*',                  -- Multiple
    '^(.-):(%d+):.*',                        -- Multiple
    '^--> (.-):(%d+):(%d+)',                 -- Rust
    '^  File "([^"]+)", line (%d+), in .*',  -- Python
  }
  for _, pattern in ipairs(patterns) do
    local file, line_num, col_num = string.match(line, pattern)
    line_num, col_num = tonumber(line_num), tonumber(col_num)
    if file then
      file = vim.fn.fnameescape(file)
      if vim.fn.filereadable(file) == 1 and line_num then
        col_num = col_num or 1
        vim.cmd("silent close | silent e " .. file)
        vim.fn.setcursorcharpos(line_num, col_num)
        return
      end
    end
  end
end

local Terminal = {}
Terminal.__index = Terminal

function Terminal.new(cmd)
  cmd = cmd or "$SHELL"
  return setmetatable({ autocmds = {}, cmd = cmd, }, Terminal)
end

function Terminal:open(cmd)
  local win_config = function()
    local col_margin = math.floor(vim.o.columns * 0.1)
    local row_margin = math.floor(vim.o.lines * 0.1)
    return {
      row = row_margin,
      col = col_margin,
      height = vim.o.lines - row_margin * 2 - 2,
      width = vim.o.columns - col_margin * 2 - 2,
      border = "rounded",
      style = 'minimal',
      relative = 'editor',
    }
  end

  local open_win = function(buf)
    return vim.api.nvim_open_win(buf, true, win_config())
  end

  if self.buf and self.win and vim.api.nvim_win_is_valid(self.win) then
    return
  end

  if self.buf then
    self.win = open_win(self.buf)
    return
  end

  self.buf = vim.api.nvim_create_buf(false, true)
  self.win = open_win(self.buf)
  self.job_id = vim.fn.jobstart(self.cmd, {
    term = true,
    on_exit = function()
      for i, autocmd in ipairs(self.autocmds) do
        if autocmd then
          vim.api.nvim_del_autocmd(autocmd)
          self.autocmds[i] = nil
        end
      end

      self:close()

      if self.buf and vim.api.nvim_buf_is_valid(self.buf) then
        vim.api.nvim_buf_delete(self.buf, { force = true })
        self.buf = nil
      end

      self.job_id = nil
    end,
  })
  vim.cmd "startinsert"

  self.autocmds[1] = vim.api.nvim_create_autocmd("WinLeave", {
    callback = function()
      self:close()
    end})

    self.autocmds[2] = vim.api.nvim_create_autocmd("WinResized", {
      callback = function()
        if self.win and vim.api.nvim_win_is_valid(self.win) then
          vim.api.nvim_win_set_config(self.win, win_config())
        end
      end,
    })
end

function Terminal:close()
  if self.win and vim.api.nvim_win_is_valid(self.win) then
    vim.api.nvim_win_close(self.win, true)
    self.win = nil
  end
end

local terminals = {}

local function open_terminal(id)
  if not terminals[id] or vim.fn.jobwait({ terminals[id] }, 0) ~= -1 then
    terminals[id] = Terminal.new()
  end
  terminals[id]:open()
end

for i=1,6 do
  local id = tostring(i)
  vim.keymap.set("n", "<leader>t"..id, function() open_terminal(id) end)
end

local Codex = setmetatable({}, { __index = Terminal })
Codex.__index = Codex

function Codex.new()
  local t = Terminal.new("codex --sandbox workspace-write --ask-for-approval untrusted --search")
  t.ready = false
  t.pending = {}
  return setmetatable(t, Codex)
end

function Codex:open()
  local i = 0

  local wait_for_prompt
  wait_for_prompt = function()
    local lines = vim.api.nvim_buf_get_lines(self.buf, 0, -1, true)
    if string.find(vim.fn.join(lines, "\n"), "OpenAI Codex.*\n›") then
      self.started = true
      self:send(self.pending)
      self.pending = {}
    elseif i < 100 then
      i = i + 1
      vim.defer_fn(wait_for_prompt, 50)
    end
  end

  Terminal.open(self)
  if not self.started then
    wait_for_prompt()
  end
end

function Codex:send_file_lines(file, line1, line2)
  if not vim.uv.fs_stat(file) then return end

  local data = file
  if line1 then
    line2 = line2 or line1
    if line1 > line2 then line1, line2 = line2, line1 end

    if line1 == line2 then
      data = string.format("%s:%d", data, line1)
    else
      data = string.format("%s:%d-%d", data, line1, line2)
    end
  end

  self:send(data)
end

function Codex:send(lines)
  if type(lines) == 'string' then lines = { lines } end
  if self.started then
    if #lines > 0 then
      vim.fn.chansend(self.job_id, vim.fn.join(lines, "\n") .. "\n")
    end
  else
    self.pending = vim.fn.extend(self.pending, lines)
  end
end

local function open_codex()
  if not terminals.codex or vim.fn.jobwait({ terminals.codex }, 0) ~= -1 then
    terminals.codex = Codex.new()
  end
  terminals.codex:open()
end

vim.keymap.set("n", "<leader>tcc", open_codex)

vim.keymap.set("n", "<leader>tcf", function()
  local file = vim.fn.expand("%")
  open_codex()
  terminals.codex:send_file_lines(file)
end)

vim.keymap.set("n", "<leader>tcl", function()
  local file = vim.fn.expand("%")
  local line = vim.fn.line(".")
  open_codex()
  terminals.codex:send_file_lines(file, line)
end)

vim.keymap.set("x", "<leader>tcl", function()
  local file = vim.fn.expand("%")
  local pos1 = vim.fn.getpos('.')
  local pos2 = vim.fn.getpos('v')
  local line1 = pos1[2]
  local line2 = pos2[2]
  open_codex()
  terminals.codex:send_file_lines(file, line1, line2)
end)
