local function mini_spec(spec, no_very_lazy)
  local result = { version = false, opts = {} }
  if not no_very_lazy then
    result.event = "VeryLazy"
  end
  result = vim.tbl_deep_extend('force', result, spec or {})
  return result
end

local function mini(name, spec)
  local ret = { 'echasnovski/mini.'..name, }
  for k, v in pairs(spec or mini_spec()) do
    ret[k] = v
  end
  return ret
end

local bufremove_spec = mini_spec({
  keys = {
    { "<leader>bd", "<cmd>lua require('mini.bufremove').delete()<cr>", desc = "Delete buffer smartly" },
    { "<leader>bw", "<cmd>lua require('mini.bufremove').wipeout()<cr>", desc = "Wipeout buffer smartly" },
  },
  cmd = { "Bd", "BD", "Bw", "BW" },
  init = function()
    vim.cmd [[cabbrev bd BD]]
    vim.cmd [[cabbrev bw BW]]
  end,
  config = function()
    local bufremove = require("mini.bufremove")
    bufremove.setup()
    for _, cmd in ipairs({{{ "Bd", "BD"}, "delete"}, {{ "Bw", "BW"}, "wipeout"}}) do
      for _, c in ipairs(cmd[1]) do
        vim.api.nvim_create_user_command(c, function(arg)
          local buf = arg.fargs[1] and (tonumber(arg.fargs[1]) or vim.fn.bufnr(arg.fargs[1]))
          bufremove[cmd[2]](buf, arg.bang)
        end, { bang = true, complete = 'buffer', nargs = '?', desc = cmd[2] .. " buffer smartly"})
      end
    end
  end,
}, true)

local files_spec = mini_spec({
  keys = { { "<leader>ff", "<cmd>lua require('mini.files').open()<cr>", desc = "Open mini files" } },
}, true)

local hipatterns_spec = mini_spec {
  opts = function()
    local hipatterns = require("mini.hipatterns")
    return {
      highlighters = {
        -- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
        fixme = { pattern = '%f[%w]()FIXME()%f[%W]', group = 'MiniHipatternsFixme' },
        hack  = { pattern = '%f[%w]()HACK()%f[%W]',  group = 'MiniHipatternsHack'  },
        todo  = { pattern = '%f[%w]()TODO()%f[%W]',  group = 'MiniHipatternsTodo'  },
        note  = { pattern = '%f[%w]()NOTE()%f[%W]',  group = 'MiniHipatternsNote'  },

        -- Highlight hex color strings (`#rrggbb`) using that color
        hex_color = hipatterns.gen_highlighter.hex_color(),
      },
    }
  end
}

local move_spec = mini_spec { opts = { mappings = { line_left = '', line_right = '', } } }

return {
  mini("ai"),
  mini("bracketed"),
  mini("bufremove", bufremove_spec),
  mini("comment"),
  mini("files", files_spec),
  mini("fuzzy"),
  mini("hipatterns", hipatterns_spec),
  mini("jump"),
  mini("move", move_spec),
  mini("operators"),
  -- mini("pairs"),
  mini("splitjoin"),
  mini("trailspace"),
}
