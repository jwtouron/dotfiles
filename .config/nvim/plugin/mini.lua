local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

local function mini(name)
  return { src = "https://github.com/nvim-mini/mini." .. name, version = "stable" }
end

vim.pack.add(
  {
    mini("bufremove"),
    mini("hipatterns"),
    { src = "https://github.com/rafamadriz/friendly-snippets" },
    mini("snippets"),
    mini("trailspace"),
  }, { confirm = false }
)

for _, cmd in ipairs({ "delete", "wipeout" }) do
  vim.api.nvim_create_user_command(
    "B" .. cmd,
    function(arg)
      local buf_id = 0
      if arg.args ~= "" then
        buf_id = tonumber(arg.args) or vim.fn.bufnr(arg.args)
        if buf_id == -1 then
          error(string.format("Invalid buffer: %s", arg.args))
        end
      end
      require("mini.bufremove")[cmd](buf_id, arg.bang)
    end,
    {
      complete = "buffer",
      nargs = "?",
      bang = true,
    }
  )
  vim.cmd.cabbr("b" .. cmd:sub(1, 1), "B" .. cmd)
end

local hipatterns = require('mini.hipatterns')
hipatterns.setup {
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

local gen_loader = require('mini.snippets').gen_loader
require('mini.snippets').setup({
  snippets = {
    -- gen_loader.from_file('~/.config/nvim/snippets/global.json'),
    -- "snippets/" subdirectories from 'runtimepath' directories.
    gen_loader.from_lang(),
  },
})

require("mini.trailspace").setup()
local setup_highlight = function()
  vim.api.nvim_set_hl(0, "MiniTrailspace", { sp = "#FA8072", undercurl = true, force = true, nocombine = true, })
end
setup_highlight()
vim.api.nvim_create_autocmd("ColorScheme", {
  group = augroup,
  callback = function() vim.schedule(setup_highlight) end,
})
