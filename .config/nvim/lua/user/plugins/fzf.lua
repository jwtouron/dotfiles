return {
  "ibhagwan/fzf-lua",
  dependencies = "nvim-tree/nvim-web-devicons",
  cmd = "FzfLua",
  keys = function()
    local fzf_lua = require('fzf-lua')

    local make_cwd_func = function(func)
      return function()
        local ok, dir = pcall(vim.fn.input, "Directory: ", "", "dir")
        if ok then
          if vim.fn.isdirectory(dir) ~= 0 then
            func({ cwd = dir })
          else
            vim.notify("`" .. dir .. "` is not a directory", vim.log.levels.WARN)
          end
        end
      end
    end

    return {
      { "<leader><space>", fzf_lua.files, desc = "FZF Files" },
      { "<leader>,", fzf_lua.buffers, desc = "FZF Buffers" },
      { "<leader>/", fzf_lua.blines, desc = "FZF Buffer Lines" },
      { "<leader>]", fzf_lua.tags, desc = "FZF Tags" },

      { "<leader>zc", fzf_lua.command_history, desc = "FZF Command History" },
      { "<leader>zd", fzf_lua.diagnostics_document, desc = "FZF Diagnostics Document" },
      { "<leader>zD", fzf_lua.diagnostics_workspace, desc = "FZF Diagnostics Workspace" },
      { "<leader>zf", fzf_lua.files, desc = "FZF Files" },
      { "<leader>zF", make_cwd_func(fzf_lua.files), desc = "FZF Files (specify cwd)" },
      { "<leader>zg", fzf_lua.live_grep_glob, desc = "FZF Live Grep Glob" },
      { "<leader>zG", make_cwd_func(fzf_lua.live_grep_glob), desc = "FZF Live Grep Glob (specify cwd)" },
      { "<leader>zh", fzf_lua.helptags, desc = "FZF Help Tags" },
      { "<leader>zk", fzf_lua.keymaps, desc = "FZF Keymaps" },
      { "<leader>zl", fzf_lua.loclist, desc = "FZF Loclist" },
      { "<leader>zm", fzf_lua.marks, desc = "FZF Marks" },
      { "<leader>zM", fzf_lua.manpages, desc = "FZF Man Pages" },
      { "<leader>zo", fzf_lua.oldfiles, desc = "FZF Old Files" },
      { "<leader>zq", fzf_lua.quickfix, desc = "FZF Quickfix" },
      { "<leader>zr", fzf_lua.registers, desc = "FZF Registers" },
      { "<leader>zR", fzf_lua.resume, desc = "FZF Resume" },
      { "<leader>zs", fzf_lua.spell_suggest, desc = "FZF Spell Suggest" },
      { "<leader>zz", fzf_lua.builtin, desc = "FZF Builtin" },

      { "<leader>lc", fzf_lua.lsp_code_actions, desc = "FZF LSP Code Actions" },
    }
  end,
  opts = {
    winopts = {
      on_create = function()
        local opts = { nowait = true, buffer = true }
        vim.keymap.set("t", "<C-b>", "<Left>", opts)
        vim.keymap.set("t", "<C-f>", "<Right>", opts)
      end
    }
  },
}
