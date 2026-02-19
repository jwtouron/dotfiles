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

local term_codex
local setup_term_codex
setup_term_codex = function()
  setup()
  term_codex = Terminal:new {
    id = 7,
    cmd = "codex --sandbox workspace-write --ask-for-approval on-request --search",
    direction = "float",
  }
  setup_term_codex = function() end
end

local function send(first_line, last_line)
  if first_line and last_line and first_line > last_line then
    first_line, last_line = last_line, first_line
  end
  local filename = vim.api.nvim_buf_get_name(0)
  if filename ~= "" and vim.fn.filereadable(filename) ~= 0 then
    filename = vim.fn.fnamemodify(filename, ':.')
    term_codex:open()
    term_codex:send(string.format("%s%s%s", filename, first_line and ":" .. first_line or "", last_line and "-" .. last_line or ""))
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

local function keymap_set(mode, lhs, rhs, opts)
  vim.keymap.set(mode, lhs, function() setup(); rhs() end, opts)
end

local function keymap_set_codex(mode, lhs, rhs, opts)
  vim.keymap.set(mode, lhs, function() setup_term_codex(); rhs() end, opts)
end

keymap_set_codex("n", "<leader>tcc", function() term_codex:toggle() end)
keymap_set_codex("n", "<leader>tcf", send)
keymap_set_codex("n", "<leader>tcl", function() send(vim.fn.line('.')) end)
keymap_set_codex("x", "<leader>tcl", send_visual)

keymap_set("n", "<leader>t1", function() vim.cmd("1ToggleTerm direction=float") end)
keymap_set("n", "<leader>t2", function() vim.cmd("2ToggleTerm direction=float") end)
keymap_set("n", "<leader>t3", function() vim.cmd("3ToggleTerm direction=float") end)
keymap_set("n", "<leader>t4", function() vim.cmd("4ToggleTerm direction=float") end)
keymap_set("n", "<leader>t5", function() vim.cmd("5ToggleTerm direction=float") end)
keymap_set("n", "<leader>t6", function() vim.cmd("6ToggleTerm direction=float") end)
