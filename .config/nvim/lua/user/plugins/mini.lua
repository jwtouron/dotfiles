local augroup = vim.api.nvim_create_augroup("user.plugins.mini", { clear = true })

local function mini(name, spec)
  local ret = {
    "echasnovski/mini." .. name,
    version = false,
  }
  for k, v in pairs(spec or { event = "VeryLazy", opts = {} }) do
    ret[k] = v
  end
  return ret
end

local bufremove_spec = {
  event = "CmdlineEnter",
  opts = {},
  init = function()
    vim.cmd.cabbr "bd lua require('mini.bufremove').delete()<Left>"
    vim.cmd.cabbr "bw lua require('mini.bufremove').wipeout()<Left>"
  end,
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

        { mode = "n", keys = "<leader><space>", desc = "FZF Files" },
        { mode = "n", keys = "<leader>,", desc = "FZF Buffers" },

        { mode = "n", keys = "<leader>e", desc = "[E]xec" },
        { mode = "n", keys = "<leader>h", desc = "Quick [H]istory" },
        { mode = "n", keys = "<leader>l", desc = "[L]SP" },
        { mode = "n", keys = "<leader>o", desc = "[O]il" },
        { mode = "n", keys = "<leader>z", desc = "F[Z]F" },
      },
    })
  end
}

local files_spec = {
  opts = {},
  keys = {
    { "<leader>ff", "<cmd>lua require('mini.files').open()<cr>", desc = "Mini Files" },
  },
  config = function()
    vim.api.nvim_create_autocmd("FileType", {
      group = augroup,
      pattern = "minifiles",
      callback = function()
        vim.keymap.set("n", "!", function()
          local cword = vim.fn.expand('<cWORD>')
          require('mini.files').close()
          return ":grep" .. cword .. "<Home><Right><Right><Right><Right><Del>  <Left>"
        end,
        { buffer = true, expr = true, })
      end,
    })
  end,
}

local trailspace_spec = {
  opts = {},
  init = function()
    vim.api.nvim_create_autocmd("ColorScheme", {
      group = augroup,
      pattern = "*",
      command = "highlight MiniTrailspace guifg=salmon guisp=salmon gui=undercurl cterm=undercurl guibg=NONE ctermbg=NONE"
    })
  end,
}

return {
  mini('ai'),
  mini('bracketed'),
  mini('bufremove', bufremove_spec),
  mini('clue', clue_spec),
  -- mini('comment'),
  -- mini('files', files_spec),
  -- mini('statusline', { opts = {} }),
  mini('trailspace', trailspace_spec),
}
