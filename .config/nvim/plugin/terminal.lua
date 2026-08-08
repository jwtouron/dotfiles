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
        vim.cmd("silent close | silent edit " .. file)
        vim.fn.setcursorcharpos(line_num, col_num)
        return
      end
    end
  end
end

local Terminal = {}
Terminal.__index = Terminal

function Terminal.new(cmd, args)
  local self = {
    cmd = cmd or vim.fn.getenv("SHELL"),
    args = args or {},
    autocmds = {},
  }
  return setmetatable(self, Terminal)
end

function Terminal:open()
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
    local win = vim.api.nvim_open_win(buf, true, win_config())
    vim.cmd "startinsert"
    return win
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
  self.job_id = vim.fn.jobstart(vim.fn.extendnew({ self.cmd }, self.args), {
    term = true,
    on_exit = function()
      self:close()  -- Close window

      if self.buf and vim.api.nvim_buf_is_valid(self.buf) then
        vim.api.nvim_buf_delete(self.buf, { force = true })
        self.buf = nil
      end

      self.job_id = nil
    end,

  })

  vim.api.nvim_create_autocmd("WinLeave", {
    callback = function()
      if vim.fn.bufnr() == self.buf then
        self:close()
      end
    end,
    buffer = self.buf,
  })

  vim.api.nvim_create_autocmd("VimResized", {
    callback = function()
      if self.win and vim.api.nvim_win_is_valid(self.win) then
        vim.api.nvim_win_set_config(self.win, win_config())
      end
    end,
    buffer = self.buf,
  })

  vim.keymap.set("n", "<cr>", goto_file_line_col, { buf = self.buf })
end

function Terminal:close()
  if self.win and vim.api.nvim_win_is_valid(self.win) then
    vim.api.nvim_win_close(self.win, true)
    self.win = nil
  end
end

function Terminal:is_running()
  return self.job_id ~= nil and vim.fn.jobwait({ self.job_id }, 0)[1] == -1
end

local terminals = {}

local function is_running(term)
  return term and term:is_running()
end

local function open_terminal(id)
  if not is_running(terminals[id]) then
    terminals[id] = Terminal.new()
  end
  terminals[id]:open()
end

for i=1,6 do
  local id = tostring(i)
  vim.keymap.set("n", "<leader>t"..id, function() open_terminal(id) end)
end

-----------------------------
-- Agent-specific terminal --
-----------------------------

local agent_specs = {
  codex = {
    name = "codex",
    command = "codex",
    args = { "--sandbox",  "workspace-write", "--ask-for-approval", "untrusted", "--search", },
    ready_regex =  "OpenAI Codex.*\n›",
  },
  claude = {
    name = "claude",
    command = "claude",
    args = { "--permission-mode", "acceptEdits", },
    ready_regex = "Claude Code.*\n❯",
  },
}

local session_agent = nil

local function select_agent()
  local available_agents = {}
  for _, spec in pairs(agent_specs) do
    if vim.fn.executable(spec.command) == 1 then
      table.insert(available_agents, spec)
    end
  end

  if #available_agents == 0 then
    vim.notify("No agents found on system", vim.log.levels.ERROR)
    return
  end

  vim.ui.select(
    vim.fn.map(available_agents, "v:val.name"),
    { prompt = "Agent: ", },
    function(name)
      if not name or name == "" then
        vim.notify("No agent selected", vim.log.levels.INFO)
        return
      end
      assert(agent_specs[name])

      session_agent = agent_specs[name]
    end
  )
end

local function get_agent_spec()
  if session_agent then return session_agent end

  select_agent()
  return session_agent
end

local Agent = setmetatable({}, { __index = Terminal })
Agent.__index = Agent

function Agent.new(spec)
  local t = Terminal.new(spec.command, spec.args)
  t.ready = false
  t.starting = false
  t.pending = {}
  t.agent_spec = spec
  return setmetatable(t, Agent)
end

function Agent:open()
  local i = 0

  local wait_for_prompt
  wait_for_prompt = function()
    local lines = vim.api.nvim_buf_get_lines(self.buf, 0, -1, true)
    if string.find(vim.fn.join(lines, "\n"), self.agent_spec.ready_regex) then  -- NOTE: This code may break on more complicated regexes.
      self.ready = true
      self.starting = false
      self:send(self.pending)
      self.pending = {}
    elseif i < 100 then
      i = i + 1
      vim.defer_fn(wait_for_prompt, 50)
    end
  end

  Terminal.open(self)
  if not self.ready and not self.starting then
    self.starting = true
    wait_for_prompt()
  end
end

function Agent:send_file_lines(file, line1, line2)
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

function Agent:send(lines)
  if type(lines) == 'string' then lines = { lines } end
  if self.ready then
    if #lines > 0 then
      vim.fn.chansend(self.job_id, vim.fn.join(lines, "\n") .. "\n")
    end
  else
    self.pending = vim.fn.extendnew(self.pending, lines)
  end
end

local function open_agent()
  if not is_running(terminals.agent) then
    local agent_spec = get_agent_spec()
    if not agent_spec then return end
    terminals.agent = Agent.new(agent_spec)
  end
  terminals.agent:open()
end

vim.keymap.set("n", "gaa", open_agent)

vim.keymap.set("n", "gaf", function()
  local file = vim.fn.expand("%")
  open_agent()
  terminals.agent:send_file_lines(file)
end)

vim.keymap.set("n", "gal", function()
  local file = vim.fn.expand("%")
  local line = vim.fn.line(".")
  open_agent()
  terminals.agent:send_file_lines(file, line)
end)

vim.keymap.set("x", "gal", function()
  local file = vim.fn.expand("%")
  local pos1 = vim.fn.getpos('.')
  local pos2 = vim.fn.getpos('v')
  local line1 = pos1[2]
  local line2 = pos2[2]
  open_agent()
  terminals.agent:send_file_lines(file, line1, line2)
end)
