local function mini(name, spec)
  local result = {
    'echasnovski/mini.'..name,
    version = false,
    event = "VeryLazy",
  }
  for k, v in pairs(spec or { config = true }) do
    result[k] = v
  end
  return result
end

local function ai_spec()
  return {
    dependencies = "nvim-treesitter/nvim-treesitter-textobjects",
    opts = function()
      local ai = require("mini.ai")
      return {
        custom_textobjects = {
          o = ai.gen_spec.treesitter({
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }, {}),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }, {}),
          c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }, {}),
        },
      }
    end
  }
end

local function move_spec()
  return {
    opts = {
      mappings = {
        line_left = '',
        line_right = '',
      }
    }
  }
end

return {
  mini("ai", ai_spec()),
  mini("align"),
  mini("bracketed"),
  mini("bufremove"),
  mini("comment"),
  mini("jump"),
  mini("move", move_spec()),
  mini("pairs"),
  mini("splitjoin"),
  mini("tabline"),
  mini("trailspace"),
}
