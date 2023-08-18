-- TODO:
-- 1. table conveniences
-- 2. check list conceal
-- 3. insert enter new list item
-- 4. move to lua/
-- 5. InsertEnter for tables and checklists
-- 6. Conceal headings and list item markers
-- 7. block_continuation for C-c/task_list
-- 8. enter on any part of link task_list_item goes to link

local ts = vim.treesitter

local function get_node_text(node)
  return ts.get_node_text(node, 0)
end

local function consumer(co)
  return function()
    while true do
      local _, value = coroutine.resume(co)
      return value
    end
  end
end

---Filter's out nodes whose type matches regex.
--
---@param f function The iterator which will be filtered.
---@param regex string The regex which will be used to match the type
---@return function
local function filter_type(f, regex)
  local co = coroutine.create(function()
    for node in f do
      if node:type():match(regex) then
        coroutine.yield(node)
      end
    end
  end)
  return consumer(co)
end

---Returns an iterator over all the node's parents.
--
---@param node userdata The node whose parents will be iterated over
---@return function
local function iter_parents(node)
  local co = coroutine.create(function()
    local parent = node:parent()
    while parent do
      coroutine.yield(parent)
      parent = parent:parent()
    end
  end)
  return consumer(co)
end

local function iter_prev_named_siblings(node)
  local co = coroutine.create(function()
    local sib = node:prev_named_sibling()
    while sib do
      coroutine.yield(sib)
      sib = sib:prev_named_sibling()
    end
  end)
  return consumer(co)
end

local function iter_next_named_siblings(node)
  local co = coroutine.create(function()
    local sib = node:next_named_sibling()
    while sib do
      coroutine.yield(sib)
      sib = sib:next_named_sibling()
    end
  end)
  return consumer(co)
end

local function first(f)
  for node in f do
    return node
  end
end

-- markdown_line with coneal links
vim.opt_local.conceallevel = 2

-- If cursor is on link, open it.
local function open_link()
  -- Default "open" command.
  local opencmd = nil
  if vim.fn.has("macunix") == 1 then
    opencmd = "open"
  elseif vim.fn.has("linux") == 1 then
    opencmd = "xdg-open"
  -- TODO: Windows: Probably start <program> or cmd /c start <program> (can't check now)
  end

  -- Get 'link_destination', if node is type 'inline_link'
  local f = function(node)
    if node then
      local dest = first(filter_type(node:iter_children(), '^link_destination$'))
      if dest then
        return get_node_text(dest)
      end
    end
  end

  local node = ts.get_node({ ignore_injections = false })
  local destination = node and (f(node) or f(node:parent()))
  if destination then
    if destination:find("^http") then
      vim.fn.system(opencmd .. " " .. destination)
    else
      vim.cmd("edit " .. destination)
    end
    return true
  end
end

-- Enter opens links
vim.keymap.set("n", "<cr>", function()
  if not open_link() then
    vim.cmd [[execute "normal!" "\<cr>"]]
  end
end, { buffer = true })


local function find_task_list_marker(node)
  if not node then
    return
  end
  if node:type() == 'list_item' then
    return first(filter_type(node:iter_children(), "^task_list_marker_"))
  end
  for parent in iter_parents(node) do
    if parent:type():match("^task_list_marker_") then
      return parent
    elseif parent:type() == 'list_item' then
      return first(filter_type(parent:iter_children(), "^task_list_marker_"))
    elseif parent:type() == 'section' or parent:type() == 'block_continuation' then
      return
    end
  end
end

local function set_task_list_marker_type(task_list_marker, newtype)
  local replacement = nil
  if newtype == "task_list_marker_checked" then
    replacement = { "[x]" }
  elseif newtype == "task_list_marker_unchecked" then
    replacement = { "[ ]" }
  end
  if replacement then
    local start_row, start_col, end_row, end_col = task_list_marker:range()
    vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, replacement)
  end
end

local function flip_toggle_task_list_marker_type(type)
  if type == 'task_list_marker_checked' then
    return 'task_list_marker_unchecked'
  elseif type == 'task_list_marker_unchecked' then
    return 'task_list_marker_checked'
  end
end

local function fixup_children(task_list_marker, type)
  type = type or flip_toggle_task_list_marker_type(task_list_marker:type())
  local list = first(filter_type(iter_next_named_siblings(task_list_marker), "^list$"))
  if not list then return end
  for children in filter_type(list:iter_children(), "^list_item$") do
    local tlm = first(filter_type(children:iter_children(), "^task_list_marker_"))
    if tlm then
      set_task_list_marker_type(tlm, type)
      fixup_children(tlm, type)
    end
  end
end

---Fixes up parents by checking them only if all children are checked.
--
---@param task_list_marker userdata Task list marker node.
---@return nil
local function fixup_parents(task_list_marker)
  local find_task_list_marker0 = function(f)
    return first(filter_type(f, "^task_list_marker_"))
  end

  for list in filter_type(iter_parents(task_list_marker), "^list$") do
    local tlm = find_task_list_marker0(iter_prev_named_siblings(list))
    if tlm then
      local type = 'task_list_marker_checked'
      for list_item in list:iter_children() do
        local tlm0 = find_task_list_marker0(list_item:iter_children())
        if tlm0 then
          if (tlm0:type() == 'task_list_marker_unchecked' and not tlm0:has_changes())
            or (tlm0:type() == 'task_list_marker_checked' and tlm0:has_changes()) then
            type = 'task_list_marker_unchecked'
            break
          end
        end
      end
      set_task_list_marker_type(tlm, type)
    end
  end
end

-- Control-C ticks/unticks check boxes
vim.keymap.set("n", "<C-c>", function()
  local task_list_marker = find_task_list_marker(ts.get_node())
  if task_list_marker then
    local task_list_marker_type = flip_toggle_task_list_marker_type(task_list_marker:type())
    set_task_list_marker_type(task_list_marker, task_list_marker_type)
    fixup_children(task_list_marker)
    fixup_parents(task_list_marker)
  end
end, { buffer = true })
