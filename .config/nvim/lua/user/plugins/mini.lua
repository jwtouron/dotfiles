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

local bufremove_spec = mini_spec {
  keys = {
    { "<leader>bd", "<cmd>lua require('mini.bufremove').delete()<cr>", desc = "Delete buffer smartly" },
    { "<leader>bw", "<cmd>lua require('mini.bufremove').wipeout()<cr>", desc = "Wipeout buffer smartly" },
  },
  config = function()
    require("mini.bufremove").setup()
    for _, cmd in ipairs({ "Bd", "BD" }) do
      vim.api.nvim_create_user_command(cmd, [[lua require('mini.bufremove').delete()]], { desc = "Delete buffer smartly" })
    end
    for _, cmd in ipairs({ "Bw", "BW" }) do
      vim.api.nvim_create_user_command(cmd, [[lua require('mini.bufremove').wipeout()]], { desc = "Wipeout buffer smartly" })
    end
  end
}

local files_spec = mini_spec {
  keys = { { "<leader>ff", "<cmd>lua require('mini.files').open()<cr>", desc = "Open mini files" } },
}

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
  -- mini("jump"),
  mini("move", move_spec),
  mini("pairs"),
  mini("splitjoin"),
  mini("trailspace"),
}
