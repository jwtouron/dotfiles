local function mini(name, spec)
  local ret = { 'echasnovski/mini.'..name, version = false, config = true }
  for k, v in pairs(spec or { event = "VeryLazy" }) do
    ret[k] = v
  end
  return ret
end

local bufremove_spec = {
  keys = {
    { "<leader>bd", "<cmd>lua require('mini.bufremove').delete()<cr>", desc = "Delete buffer smartly" },
    { "<leader>bw", "<cmd>lua require('mini.bufremove').wipeout()<cr>", desc = "Wipeout buffer smartly" },
  },
  init = function()
    vim.cmd [[cabbrev bd lua require("mini.bufremove").delete()<left>]]
    vim.cmd [[cabbrev bw lua require("mini.bufremove").wipeout()<left>]]
  end,
  config = true,
}

local clue_spec = {
  event = "VeryLazy",
  config = function()
    local miniclue = require('mini.clue')
    miniclue.setup({
      triggers = {
        -- Leader triggers
        { mode = 'n', keys = '<Leader>' },
        { mode = 'x', keys = '<Leader>' },

        -- Built-in completion
        { mode = 'i', keys = '<C-x>' },

        -- `g` key
        { mode = 'n', keys = 'g' },
        { mode = 'x', keys = 'g' },

        -- Marks
        { mode = 'n', keys = "'" },
        { mode = 'n', keys = '`' },
        { mode = 'x', keys = "'" },
        { mode = 'x', keys = '`' },

        -- Registers
        { mode = 'n', keys = '"' },
        { mode = 'x', keys = '"' },
        { mode = 'i', keys = '<C-r>' },
        { mode = 'c', keys = '<C-r>' },

        -- Window commands
        { mode = 'n', keys = '<C-w>' },

        -- `z` key
        { mode = 'n', keys = 'z' },
        { mode = 'x', keys = 'z' },
      },

      clues = {
        -- Enhance this by adding descriptions for <Leader> mapping groups
        miniclue.gen_clues.builtin_completion(),
        miniclue.gen_clues.g(),
        miniclue.gen_clues.marks(),
        miniclue.gen_clues.registers(),
        miniclue.gen_clues.windows(),
        miniclue.gen_clues.z(),

        { mode = "n", keys = "<leader>b", desc = "[B]uffer" },
        { mode = "n", keys = "<leader>e", desc = "[E]Executor" },
        { mode = "n", keys = "<leader>f", desc = "[F]ile" },
        { mode = "n", keys = "<leader>t", desc = "[T]elescope" },
        { mode = "n", keys = "<leader>w", desc = "[W]indow" },
        { mode = "n", keys = "<leader>x", desc = "Trouble" },
      },
    })
  end
}

local comment_spec = {
  keys = {
    { "gc", nil, mode = {"n", "x", "o"}, desc = "comment" },
    { "gcc", nil, mode = "n", desc = "comment line" }
  }
}

local completion_spec = {
  event = "VeryLazy",
  keys = {
    { '<Tab>',   [[pumvisible() ? "\<C-n>" : "\<Tab>"]],   mode = 'i', expr = true },
    { '<S-Tab>', [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]], mode = 'i', expr = true },
    { 'jk', [[pumvisible() ? "<esc>" : "jk"]], mode = 'i', expr = true },
    { 'kj', [[pumvisible() ? "<esc>" : "kj"]], mode = 'i', expr = true },
  },
  opts = {
    delay = { completion = 200, info = 200, signature = 100 },
    -- delay = { completion = 100, info = 100, signature = 50 },
    window = {
      -- nvim_open_win
      -- none, single, double, rounded, solid, shadow
      info = { border = 'rounded' },
      signature = { border = 'rounded' },
    },
  },
}

local mini_files = {
  "jwtouron/mini.files",
  branch = "execute-command",
  keys = {
    { "<leader>ff", "<cmd>lua require('mini.files').open()<cr>", desc = "Open mini files" }
  },
}

local files_spec = {
  keys = { { "<leader>ff", "<cmd>lua require('mini.files').open()<cr>", desc = "Open mini files" } },
}

local hipatterns_spec = {
  event = "VeryLazy",
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

local move_spec = { opts = { mappings = { line_left = '', line_right = '', } } }

local trailspace_spec = {
  event = "VeryLazy",
  init = function()
    vim.api.nvim_create_autocmd("ColorScheme", {
      group = MyAugroup,
      pattern = "*",
      callback = function()
        vim.cmd.highlight("MiniTrailspace guifg=salmon guisp=salmon gui=undercurl cterm=undercurl guibg=NONE ctermbg=NONE")
      end
    })
  end
}

return {
  mini("ai"),
  mini("align"),
  -- mini("bracketed"),
  mini("bufremove", bufremove_spec),
  mini("clue", clue_spec),
  mini("comment", comment_spec),
  -- mini("completion", completion_spec),
  mini_files,
  -- mini("files", files_spec),
  mini("fuzzy"),
  -- mini("hipatterns", hipatterns_spec),
  mini("jump"),
  -- mini("move", move_spec),
  -- mini("operators"),
  mini("splitjoin"),
  mini("trailspace", trailspace_spec),
}
