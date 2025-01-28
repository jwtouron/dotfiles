local function warn(msg)
  vim.notify("[RedoCommand] " .. msg, vim.log.levels.WARN)
end

local function _error(msg)
  error("[RedoCommand] " .. msg)
end

local function print_commands(commands, limit)
  limit = limit or #commands
  for i = 1, limit do
    if commands[i] then
      print(i .. ") " .. commands[i])
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
  if i > self.limit then
    _error("Index greater than limit: index: " .. i .. ", limit: " .. self.limit)
  end
  self.commands[i] = c
end

function Saved:get(i)
  if i > self.limit then
    _error("Index greater than limit: index: " .. i .. ", limit: " .. self.limit)
  end
  return self.commands[i]
end

function Saved:print()
  print_commands(self.commands, self.limit)
end

function Saved:is_empty()
  for i = 1, self.limit do
    if self.commands[i] then
      return false
    end
  end
  return true
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

M.setup = function()
  vim.api.nvim_create_user_command(
    "RC",
    function(arg)
      local last_command = vim.fn.histget('cmd', -1)
      if last_command:find('RC') then
        vim.fn.histdel('cmd', -1)
      else
        _error("Last command doesn't contain `RC`: " .. last_command)
      end

      local history = History.new()
      local command = nil

      if arg.args ~= "" then
        local commands = history:find(arg.args, 10)
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
            if not ok then return end
            index = tonumber(index)
          end
          command = commands[index]
          vim.cmd("redraw")
        end

        if arg.count >= 1 and arg.count <= saved.limit then
          saved:set(arg.count, command)
        end
      else
        if arg.count then
          command = saved:get(arg.count)
          if not command then
            warn("Invalid index: " .. arg.count)
            return
          end
        else
          local commands = history:find('.', 1)
          if #commands == 0 then
            warn("Empty history")
            return
          end
          command = commands[1]
        end
      end

      if not command then
        _error("Empty command")
        return
      end

      vim.cmd(command)
    end,
    {
      count = 0,
      nargs = "?",
    }
  )

  vim.api.nvim_create_user_command(
    "RL",
    function()
      if not saved:is_empty() then
        saved:print()
      else
        warn("No saved commands")
      end
    end,
    {
      nargs = 0,
    }
  )

  vim.api.nvim_create_user_command(
    "RD",
    function(arg)
      local indices = {}

      for _, a in ipairs(arg.fargs) do
        if a == '*' then
          saved:reset()
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
          _error("Invalid arguments: Arguments must be in the form: RD 1 2-4 5 6-7")
        end
        for i = s, e do
          table.insert(indices, i)
        end
      end

      for _, index in ipairs(indices) do
        saved:set(index, nil)
      end
    end,
    {
      nargs = '+',
    }
  )
end

return M
