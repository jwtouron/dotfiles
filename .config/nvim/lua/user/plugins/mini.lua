local function mini_spec(spec)
  return vim.tbl_deep_extend('force', { version = false, event = "VeryLazy", opts = {} }, spec or {})
end

local function mini(name, spec)
  local ret = { 'echasnovski/mini.'..name, }
  for k, v in pairs(spec or mini_spec()) do
    ret[k] = v
  end
  return ret
end

local function hipatterns_spec()
  return mini_spec {
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
end

local function move_spec()
  return mini_spec { opts = { mappings = { line_left = '', line_right = '', } } }
end

return {
  mini("ai"),
  mini("bracketed"),
  mini("bufremove"),
  mini("comment"),
  mini("fuzzy"),
  mini("hipatterns", hipatterns_spec()),
  -- mini("jump"),
  mini("move", move_spec()),
  mini("splitjoin"),
  mini("trailspace"),
}
