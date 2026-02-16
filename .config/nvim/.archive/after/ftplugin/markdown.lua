-- local function get_open_command()
--   local uname = vim.loop.os_uname().sysname
--   if uname == "Linux" then
--     return "xdg-open"
--   elseif uname == "Darwin" then
--     return "open"
--   elseif uname == "Windows_NT" then
--     return "start"
--   else
--     error("Unsupported OS: " .. uname)
--   end
-- end
--
-- local function follow_link()
--   local _, col = unpack(vim.api.nvim_win_get_cursor(0))
--   local line = vim.api.nvim_get_current_line()
--   local start_pos, end_pos, url = string.find(line, '%[.-%]%((.-)%)')
--
--   if url then
--     start_pos = start_pos - 1
--     end_pos = end_pos - 1
--
--     if col >= start_pos and col <= end_pos then
--       local open_command = get_open_command()
--       vim.fn.system(open_command .. ' ' .. vim.fn.shellescape(url))
--     end
--   end
-- end
--
-- vim.keymap.set("n", "<cr>", function() follow_link() end, {buffer = true})

local function get_indent(line)
  return #line:match('^%s*')
end

local function is_checked_task_list_item(line)
    return line:find('^%s*- %[x%]') ~= nil
end

local function is_unchecked_task_list_item(line)
    return line:find('^%s*- %[ %]') ~= nil
end

local function set_task_list_item(line_num, checked, line)
  line = line or vim.fn.getline(line_num)
  if checked then
    checked = '[x]'
  else
    checked = '[ ]'
  end
  line = line:gsub('^(%s*- )%[[ x]%]', '%1' .. checked)
  vim.api.nvim_buf_set_lines(0, line_num - 1, line_num, true, { line })
end

local function set_child_task_list_items(start_line, indent, checked)
  for i = start_line, vim.fn.line('$') do
    local line = vim.fn.getline(i)
    local current_indent = get_indent(line)

    if line:find('^%s*$') or current_indent <= indent then return end

    set_task_list_item(i, checked, line)
  end
end

local function toggle_task_list_items()
  local line_num = vim.fn.line('.')
  local line = vim.fn.getline('.')
  local indent = get_indent(line)
  local checked = nil

  if is_checked_task_list_item(line) then
    checked = false
  elseif is_unchecked_task_list_item(line) then
    checked = true
  end

  if checked == nil then return false end

  set_task_list_item(line_num, checked, line)
  -- set_child_task_list_items(line_num + 1, indent, checked)

  return true
end


local function get_fenced_code_block()
  local line_num = vim.fn.line('.')
  local begin_line = nil
  local end_line = nil
  local lang = nil

  -- Find beginning
  for i = line_num, 1, -1 do
    local line = vim.fn.getline(i)

    if i ~= line_num and line:find('^```%s*$') then return nil end

    local l, n = line:gsub('^```%s*(.+)$', '%1')
    if n > 0 then
      lang = l
      begin_line = i
      break
    end
  end

  if not begin_line then return nil end

  -- Find end
  for i = line_num, vim.fn.line('$') do
    local line = vim.fn.getline(i)

    if i ~= line_num and line:find('^```%s*.+$') then return nil end

    if line:find('^```%s*$') then
      end_line = i
      break
    end
  end

  if not end_line then return nil end

  return { begin_line = begin_line, end_line = end_line, lang = lang }
end

local handlers = {

  python = function(code)
    if vim.fn.executable('python') ~= 0 then
      return 'python -c ' .. vim.fn.shellescape(code)
    elseif vim.fn.executable('python3') ~= 0 then
      return 'python3 -c ' .. vim.fn.shellescape(code)
    else
      error("Can't find python interpreter")
    end
  end,

  sh = function(code)
    return code
  end

}

local function execute_fenced_code_block()
  local fenced_code_block = get_fenced_code_block()
  if not fenced_code_block then return false end

  local handler = handlers[fenced_code_block.lang]
  if not handler then return false end

  local code = vim.api.nvim_buf_get_lines(0, fenced_code_block.begin_line, fenced_code_block.end_line - 1, false)
  code = vim.fn.join(code, '\n')

  local command = handler(code)
  local result = vim.fn.system(command):gsub("([^\n])$", "%1\n")
  vim.api.nvim_out_write(result)
end


vim.keymap.set("n", "<c-c>", function()
  if toggle_task_list_items() then return end
  if execute_fenced_code_block() then return end
end, {buffer = true})

