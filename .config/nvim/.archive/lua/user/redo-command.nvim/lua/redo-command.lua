local function warn(msg)
  vim.notify("[RedoCommand] " .. msg, vim.log.levels.WARN)
end

local luaerror = error

local function error(msg)
  luaerror("[RedoCommand] " .. msg)
end

local function print_commands(commands, limit)
  limit = limit or #commands
  for i = 1, limit do
    if commands[i] then
      print(string.format("%2d. %s", i, commands[i]))
    end
  end
end

local Saved = {}
Saved.__index = Saved

function Saved.new(limit)
  limit = limit or 10
  return setmetatable({ commands = {}, limit = limit }, Saved)
end

function Saved:reset()
  self.commands = {}
end

function Saved:set(i, c)
  if i < 1 or i > self.limit then
    error(string.format("Invalid index: %d", i))
  end
  self.commands[i] = c
end

function Saved:get(i)
  if i < 1 or i > self.limit then
    error(string.format("Invalid index: %d", i))
  end
  return self.commands[i]
end

function Saved:print()
  print_commands(self.commands, self.limit)
end

local History = {}
History.__index = History

function History.new()
  local commands = vim.split(vim.fn.execute('history cmd'), '\n')
  return setmetatable({ commands = commands }, History)
end

function History:find(pattern, limit)
  pattern = pattern or '.'
  local commands = {}
  for i = #self.commands, 1, -1 do
    self.commands[i] = self.commands[i]:gsub("^>? *%d+ *", "")
    local m = vim.fn.matchstr(self.commands[i], ".*" .. pattern .. ".*")
    if m ~= "" then
      table.insert(commands, m)
      if limit and #commands == limit then
        break
      end
    end
  end
  return commands
end

local saved = Saved.new()

local M = {}

M.redo_command = function(pattern, limit)
  pattern = pattern or "."
  limit = limit or 10

  local last_command = vim.fn.histget('cmd', -1)
  if last_command:find('RC') then
    vim.fn.histdel('cmd', -1)
  -- else
  --   error("Last command doesn't contain `RC`: " .. last_command)
  end

  local history = History.new()
  local command = nil

  local commands = history:find(pattern, limit)
  if #commands == 0 then
    warn("No matching commands found")
    return
  end

  command = commands[1]

  if #commands > 1 then
    print_commands(commands)
    local index = nil
    while not commands[index] do
      local ok = false
      ok, index = pcall(vim.fn.input, "Type number and <Enter>: ", '1')
      if not ok then return nil end
      index = tonumber(index)
    end
    vim.cmd("redraw")
    command = commands[index]
  end

  if not command then
    error("Empty command")
    return
  end

  vim.cmd(command)

  return command
end

M.save_command = function(command, index)
  saved:set(index, command)
end

M.delete_saved_commands = function(indices)
  if not indices then
    saved:reset()
  else
    for _, index in ipairs(indices) do
      saved:set(index, nil)
    end
  end
end

M.list_saved_commands = function()
  saved:print()
end

M.setup = function()
  vim.api.nvim_create_user_command(
    "RC",
    function(arg)
      if arg.count > saved.limit then
        warn("Invalid index: " .. arg.count)
        return
      end

      if arg.args ~= "" then
        local command = M.redo_command(arg.args)
        if arg.count > 0 then
          saved:set(arg.count, command)
        end
      else
        if arg.count == 0 then
          M.redo_command('.', 1)
        else
          local command = saved:get(arg.count)
          if not command then
            warn("Invalid index: " .. arg.count)
            return
          end
          M.redo_command(command, 1)
        end
      end
    end,
    {
      count = 0,
      nargs = "?",
      desc = "Redo Command",
    }
  )

  vim.api.nvim_create_user_command(
    "RL",
    M.list_saved_commands,
    { nargs = 0, desc = "List Saved Commands", }
  )

  vim.api.nvim_create_user_command(
    "RD",
    function(arg)
      local indices = {}

      for _, a in ipairs(arg.fargs) do
        if a == '*' then
          M.delete_saved_commands()
          return
        end

        local s, e = nil, nil
        _, _, s, e = a:find("^([^-]+)-([^-]+)")
        if s then
          s = tonumber(s)
          e = tonumber(e)
        else
          s = tonumber(a)
          e = tonumber(a)
        end
        if not (s and e and s <= e) then
          warn("Invalid arguments: Arguments must be in the form: RD 1 2-4 5 6-7")
          return
        end
        for i = s, e do
          table.insert(indices, i)
        end
      end

      M.delete_saved_commands(indices)
    end,
    {
      nargs = '+',
      desc = "Delete Saved Commands",
    }
  )
end

return M
