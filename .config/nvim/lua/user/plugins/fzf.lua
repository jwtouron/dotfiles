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
  "ibhagwan/fzf-lua",
  enabled = false,
  dependencies = "nvim-tree/nvim-web-devicons",
  cmd = "FzfLua",
  keys = function()
    local fzf_lua = require('fzf-lua')

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
      end,
    },
    files = { no_ignore = true }
  },
}

-- fzf --reverse --ansi --print-query --disabled --expect=enter --bind 'start:reload:true' --bind 'change:transform:echo "reload:sleep 0.1; q={q}; if [ \"\${q/ --/}\" = \"\$q\" ]; then rg --vimgrep --color=always -e \"\$q\" 2>&1;else eval \"rg --vimgrep --color=always -e '\''\${q%% --*}'\'' \${q#* --} || true\"; fi"'

-- local win_config = {
--   relative = 'editor',
--   height = math.floor(vim.o.lines / 2),
--   width = math.floor(vim.o.columns / 2),
--   row = 5,
--   col = 5,
--   style = 'minimal',
--   border = 'rounded',
--   title = 'Files',
--   title_pos = 'center',
-- }
--
-- function FZF:run()
--   -- Create FZF command
--
--   local cmd = "fzf"
--
--   local expect_str = ""
--   for expect, _ in pairs(self.expects) do
--     if expect_str == "" then
--       expect_str = expect
--     else
--       expect_str = expect_str .. "," .. expect
--     end
--   end
--
--   if expect_str == "" then
--     vim.notify("[FZF] At least one '--expect' argument is required", vim.log.levels.WARN)
--     return
--   end
--
--   cmd = cmd .. " --expect=" .. vim.fn.shellescape(expect_str)
--
--   for opt, val in pairs(self.fzf_opts) do
--     if type(val) == 'boolean' then
--       cmd = string.format("%s %s", cmd, opt)
--     else
--       cmd = string.format("%s %s %s", cmd, opt, vim.fn.shellescape(val))
--     end
--   end
--
--   print(cmd)
--
--   local buf = vim.api.nvim_create_buf(false, true)
--   -- local bufnr = vim.fn.bufnr(buf)
--
--   local win = vim.api.nvim_open_win(buf, true, win_config)
--
--   vim.fn.jobstart(cmd, {
--     on_exit = function()
--       pcall(vim.api.nvim_win_close, win, true)
--       pcall(vim.api.nvim_buf_delete, buf, { force = true })
--     end,
--     term = true,
--   })
--   vim.cmd "startinsert"
-- end
