local window = nil
local ns_id = vim.api.nvim_create_namespace("QuickHistory")
local showing_virt_text = false
local shortcuts = { 'a', 's', 'd', 'f', 'j', 'k', 'l', ';' }

vim.cmd "highlight QuickHistory_VirtText guifg=bg guibg=fg"

local function show_virt_text(buf)
  for i=1, vim.fn.min({8, vim.fn.line('$')}) do
    vim.api.nvim_buf_set_extmark(buf, ns_id, vim.fn.line('w0') - 2 + i, 0, { virt_text = { { shortcuts[i], "QuickHistory_VirtText" } }, virt_text_pos = 'overlay' })
  end
  showing_virt_text = true
end

local function hide_virt_text(buf)
  vim.api.nvim_buf_clear_namespace(buf, ns_id, 0, -1)
  showing_virt_text = false
end

local function toggle_virt_text(buf)
  if showing_virt_text then
    hide_virt_text(buf)
  else
    show_virt_text(buf)
  end
end

local function calculate_buffer_contents(pattern)
  pattern = pattern or '.'
  local history_output = vim.fn.execute('history :')
  history_output = vim.split(history_output, '\n')
  local result = {}
  for i=#history_output, 1, -1 do
    local line = history_output[i]
    line = vim.fn.matchstr(line, [[^>\? *\d\+ \+\zs.*$]])
    if line ~= "" and vim.fn.match(line, pattern) >= 0 then
      table.insert(result, line)
    end
  end
  return result
end

local function execute_line_and_quit(lnum)
  local line = vim.fn.getline(lnum)
  if line ~= '' then
    vim.api.nvim_input(':' .. line .. '\n')
    vim.cmd('q')
  end
end

local function create_buffer()
  local buf = vim.api.nvim_create_buf(false, true)
  local bufnr = vim.fn.bufnr(buf)

  vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = bufnr })
  vim.keymap.set('n', '<CR>', function() execute_line_and_quit('.') end, { buffer = bufnr })
  vim.keymap.set('n', '<C-Space>', function() toggle_virt_text(buf) end, { buffer = bufnr })
  for i=1,8 do
    local shortcut = shortcuts[i]
    vim.keymap.set('n', shortcut, function()
      local lnum = vim.fn.line('w0') - 1 + i
      local line = vim.fn.getline(lnum)
      if showing_virt_text and line ~= '' then
        execute_line_and_quit(lnum)
      else
        vim.api.nvim_feedkeys(shortcut, 'n', true)
      end
    end, { buffer = bufnr })
  end

  vim.api.nvim_create_autocmd({ "CursorMoved", "InsertEnter", "WinScrolled", }, {
    pattern = '*',
    group = vim.api.nvim_create_augroup("QuickHistory_HideVirtText", {}),
    callback = function() hide_virt_text(buf) end,
  })

  return buf
end

local function create_window(buf)
  local row = math.floor(vim.o.lines * 0.2)
  local col = math.floor(vim.o.columns * 0.2)
  local win_config = {
    relative = 'editor',
    width = math.floor(vim.o.columns * 0.6),
    height = math.floor(vim.o.lines * 0.6),
    row = row,
    col = col,
    style = 'minimal',
    border = 'rounded',
  }
  window = vim.api.nvim_open_win(buf, true, win_config)

  -- Clear the augroup that hides the virtual text
  vim.api.nvim_create_autocmd("WinClosed", {
    group = vim.api.nvim_create_augroup("QuickHistory_WinClosed", {}),
    pattern = tostring(window),
    callback = function() vim.api.nvim_create_augroup("QuickHistory_HideVirtText", {}) end,
  })
end

local M = {}

M.open = function(pattern)
  if window and vim.api.nvim_win_is_valid(window) then return end

  local buf = create_buffer()
  local contents = calculate_buffer_contents(pattern)
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, contents)

  create_window(buf)
end

M.setup = function()
  vim.api.nvim_create_user_command("QuickHistory", "lua require('quick-history').open(<f-args>)", { nargs = 1 })
end

return M
