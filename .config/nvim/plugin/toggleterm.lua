vim.pack.add(
  { "https://github.com/akinsho/toggleterm.nvim", },
  { confirm = false, load = function() end }
)

local Terminal

local setup
setup = function()
  vim.cmd.packadd "toggleterm.nvim"
  require("toggleterm").setup()
  Terminal = require("toggleterm.terminal").Terminal
  setup = function() end
end

local Codex = {}
Codex.__index = Codex

function Codex.new()
  local self = setmetatable({}, Codex)
  self.ready = false
  self.pending = {}
  self.term = Terminal:new {
    id = 7,
    cmd = "codex --sandbox workspace-write --ask-for-approval untrusted --search",
    direction = "float",
    auto_scroll = false,
    on_stdout = function(_, _, lines)
      if self.ready then return end
      for _, line in ipairs(lines) do
        if line:find("›") then
          self.ready = true
          self:flush_pending()
          return
        end
      end
    end,
    on_exit = function()
      self.ready = false
      self.pending = {}
    end,
  }
  return self
end

function Codex:flush_pending()
  if not self.ready then return end
  local pending = self.pending
  self.pending = {}
  for _, line in ipairs(pending) do
    self.term:send(line)
  end
end

function Codex:send_file_lines(first_line, last_line)
  if first_line and last_line and first_line > last_line then
    first_line, last_line = last_line, first_line
  end
  local filename = vim.api.nvim_buf_get_name(0)
  if filename ~= "" and vim.fn.filereadable(filename) ~= 0 then
    filename = vim.fn.fnamemodify(filename, ':.')
    local line = string.format("%s%s%s", filename, first_line and ":" .. first_line or "", last_line and "-" .. last_line or "")
    if not self.ready then
      table.insert(self.pending, line)
    else
      self.term:send(line)
    end
    self.term:open()
  else
    vim.notify("No readable file associated with buffer.", vim.log.levels.INFO)
  end
end

function Codex:send_file_lines_visual()
  local pos1 = vim.fn.getpos('.')
  local pos2 = vim.fn.getpos('v')
  local lnum1 = pos1[2]
  local lnum2 = pos2[2]
  if not (lnum1 and lnum1 > 0 and lnum2 and lnum2 > 0) then
    error(string.format("invalid line numbers: %s %s", lnum1, lnum2))
  end
  self:send_file_lines(lnum1, lnum2)
end

local term_codex
local setup_term_codex

setup_term_codex = function()
  setup()
  term_codex = Codex.new()
  setup_term_codex = function() end
end

local function keymap_set_codex(mode, lhs, rhs, opts)
  vim.keymap.set(mode, lhs, function() setup_term_codex(); rhs() end, opts)
end

keymap_set_codex("n", "<leader>tcc", function() term_codex.term:toggle() end)
keymap_set_codex("n", "<leader>tcf", function() term_codex:send_file_lines() end)
keymap_set_codex("n", "<leader>tcl", function() term_codex:send_file_lines(vim.fn.line('.')) end)
keymap_set_codex("x", "<leader>tcl", function() term_codex:send_file_lines_visual() end)

local function goto_file_line_col(efm)
  return function()
    local line = vim.fn.getline('.')

    -- Try using errorformat
    local items = vim.fn.getqflist({ lines = { line }, efm = efm, })
    for _, item in ipairs(items) do
      if item.valid and item.bufnr > 0 and item.lnum > 0 then
        local col = item.col > 0 and item.col or 1
        vim.cmd("silent close | silent b " .. tostring(item.bufnr))
        vim.fn.setcursorcharpos(item.lnum, col)
        return
      end
    end

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
end

local function keymap_set_term(mode, lhs, rhs, opts)
  vim.keymap.set(mode, lhs, function()
    setup()
    local efm = vim.bo.errorformat ~= '' and vim.bo.errorformat or vim.o.errorformat
    rhs()
    vim.keymap.set('n', '<cr>', goto_file_line_col(efm), { buffer = true })
  end, opts)
end

keymap_set_term("n", "<leader>t1", function() vim.cmd("1ToggleTerm direction=float") end)
keymap_set_term("n", "<leader>t2", function() vim.cmd("2ToggleTerm direction=float") end)
keymap_set_term("n", "<leader>t3", function() vim.cmd("3ToggleTerm direction=float") end)
keymap_set_term("n", "<leader>t4", function() vim.cmd("4ToggleTerm direction=float") end)
keymap_set_term("n", "<leader>t5", function() vim.cmd("5ToggleTerm direction=float") end)
keymap_set_term("n", "<leader>t6", function() vim.cmd("6ToggleTerm direction=float") end)
