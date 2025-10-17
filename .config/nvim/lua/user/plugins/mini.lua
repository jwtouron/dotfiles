local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

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

local bracketed_spec = {
  event = "VeryLazy",
  opts = { comment = { suffix = '', }, file = { suffix = '', }, },
}

local bufremove_spec = {
  cmd = { "Bdelete", "Bwipeout", },
  init = function()
    vim.cmd.cabbr("bd", "Bdelete")
    vim.cmd.cabbr("bw", "Bwipeout")
  end,
  config = function()
    require('mini.bufremove').setup()

    local function create_user_command(name, func)
      vim.api.nvim_create_user_command(
        name,
        function(arg)
          local buf_id = 0  ---@type number?
          if arg.args ~= "" then
            buf_id = tonumber(arg.args) or vim.fn.bufnr(arg.args)
            if buf_id == -1 then
              error(string.format("Invalid buffer: %s", arg.args))
            end
          end
          require("mini.bufremove")[func](buf_id, arg.bang)
        end,
        {
          complete = "buffer",
          nargs = "?",
          bang = true,
        }
      )
    end

    create_user_command("Bdelete", "delete")
    create_user_command("Bwipeout", "wipeout")
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
        { mode = "n", keys = "<leader>l", desc = "[L]SP" },
        { mode = "n", keys = "<leader>o", desc = "[O]il" },
        { mode = "n", keys = "<leader>t", desc = "[T]oggleterm" },
        { mode = "x", keys = "<leader>t", desc = "[T]oggleterm" },
        { mode = "n", keys = "<leader>z", desc = "F[Z]F" },
      },
    })
  end
}

local files_spec = {
  keys = function()
    local open_current_file = function()
      local files, file_name = require('mini.files'), vim.api.nvim_buf_get_name(0)
      if file_name == "" then
        files.open()
      else
        files.open(file_name)
      end
    end
    return {
      { "<leader>f", function() require('mini.files').open() end, desc = "Mini Files" },
      { "<leader>F", open_current_file, desc = "Mini Files (current file)" },
    }
  end,
  dependencies = "nvim-tree/nvim-web-devicons",
  config = true,
  -- config = function()
  --   local mini_files = require('mini.files')
  --   mini_files.setup()
  --   vim.api.nvim_create_autocmd("User", {
  --     group = augroup,
  --     pattern = "MiniFilesWindowOpen",
  --     callback = function(args)
  --       vim.keymap.set("n", "!", function()
  --         local cword = vim.fn.expand('<cWORD>')
  --         return ":grep" .. cword .. "<Home><Right><Right><Right><Right><Del>  <Left>"
  --       end,
  --       { buffer = args.data.buf_id, expr = true })
  --     end
  --   })
  -- end,
}

local trailspace_spec = {
  config = function()
    require("mini.trailspace").setup()
    local setup_highlight = function()
      vim.api.nvim_set_hl(0, "MiniTrailspace", { sp = "#FA8072", undercurl = true, force = true, nocombine = true, })
    end
    setup_highlight()
    vim.api.nvim_create_autocmd("ColorScheme", {
      group = augroup,
      callback = function() vim.schedule(setup_highlight) end,
    })
  end
}

return {
  -- mini('ai'),
  -- mini('bracketed', bracketed_spec),
  mini('bufremove', bufremove_spec),
  mini('clue', clue_spec),
  -- mini('comment'),
  -- mini('files', files_spec),
  -- mini('jump2d', { keys = { "s", nil }, opts = { mappings = { start_jumping = 's' }, } }),
  -- mini('statusline', { opts = {} }),
  mini('trailspace', trailspace_spec),
}
