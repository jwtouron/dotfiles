return {
  "ibhagwan/fzf-lua",
  dependencies = "nvim-tree/nvim-web-devicons",
  cmd = "FzfLua",
  keys = {
    { "<leader><space>", function() require('fzf-lua').files() end, desc = "FZF Files" },
    { "<leader>,", function() require('fzf-lua').buffers() end, desc = "FZF Buffers" },
    { "<leader>/", function() require('fzf-lua').blines() end, desc = "FZF Buffer Lines" },
    { "<leader>]", function() require('fzf-lua').tags() end, desc = "FZF Tags" },

    { "<leader>zc", function() require('fzf-lua').command_history() end, desc = "FZF Command History" },
    { "<leader>zd", function() require('fzf-lua').diagnostics_document() end, desc = "FZF Diagnostics Document" },
    { "<leader>zD", function() require('fzf-lua').diagnostics_workspace() end, desc = "FZF Diagnostics Workspace" },
    { "<leader>zf", ":FzfLuaFiles ", desc = "FZF Files" },
    { "<leader>zg", function() require('fzf-lua').live_grep() end, desc = "FZF Live Grep" },
    { "<leader>zh", function() require('fzf-lua').helptags() end, desc = "FZF Help Tags" },
    { "<leader>zk", function() require('fzf-lua').keymaps() end, desc = "FZF Keymaps" },
    { "<leader>zl", function() require('fzf-lua').loclist() end, desc = "FZF Loclist" },
    { "<leader>zm", function() require('fzf-lua').marks() end, desc = "FZF Marks" },
    { "<leader>zM", function() require('fzf-lua').manpages() end, desc = "FZF Man Pages" },
    { "<leader>zo", function() require('fzf-lua').oldfiles() end, desc = "FZF Old Files" },
    { "<leader>zq", function() require('fzf-lua').quickfix() end, desc = "FZF Quickfix" },
    { "<leader>zr", function() require('fzf-lua').registers() end, desc = "FZF Registers" },
    { "<leader>zR", function() require('fzf-lua').resume() end, desc = "FZF Resume" },
    { "<leader>zs", function() require('fzf-lua').spell_suggest() end, desc = "FZF Spell Suggest" },
    { "<leader>zz", function() require('fzf-lua').builtin() end, desc = "FZF Builtin" },

    { "<leader>lc", function() require('fzf-lua').lsp_code_actions() end, desc = "FZF LSP Code Actions" },
  },
  opts = {
    files = {
      no_ignore = true,
    },
    grep = {
      query_delay = 300,
      rg_glob = true,
      rg_glob_fn = function(query)
        local regex, flags = query:match("^(.-)%s%-%-(.*)$")
        -- If no separator is detected will return the original query
        return (regex or query), flags
      end,
    },
    winopts = {
      on_create = function()
        local opts = { nowait = true, buffer = true }
        vim.keymap.set("t", "<C-b>", "<Left>", opts)
        vim.keymap.set("t", "<C-f>", "<Right>", opts)
      end,
      preview = { hidden = true },
    },
  },
  config = function()
    require('fzf-lua').setup()
    vim.api.nvim_create_user_command('FzfLuaFiles', function(arg)
      local dir = arg.args
      if dir == "" then
        dir = "."
      end
      require('fzf-lua').files { cwd = dir }
    end,
    {
      complete = function(arglead)
        local abspath = vim.fs.abspath(arglead)
        local dirname, basename = vim.fs.dirname(abspath), vim.fs.basename(abspath)
        return vim.fn.systemlist(string.format('fd --type dir --unrestricted . %s | fzf -f %s', vim.fn.shellescape(dirname), vim.fn.shellescape(basename)))
      end,
      nargs = '?',
    })
  end,
}

---@class FzfRunArgs
---@field stdin (fun(): (string|string[]))?
---@field fzfopts (string|fun():string)?
---@field handlers table<string, fun(completions: string[])>


-- ---@param args FzfRunArgs
-- local function fzf_run(args)
--   args.fzfopts = args.fzfopts or ''
--   if type(args.fzfopts) == 'function' then
--     args.fzfopts = args.fzfopts()
--   end
--
--   local buf = vim.api.nvim_create_buf(false, true)
--
--   local win_config = {
--     relative = 'editor',
--     height = 2 * math.floor(vim.o.lines / 3),
--     width = 2 * math.floor(vim.o.columns / 3),
--     row = 5,
--     col = 5,
--     style = 'minimal',
--     -- border = 'rounded',
--   }
--
--   local win = vim.api.nvim_open_win(buf, true, win_config)
--
--   local default_fzfopts = '--print-query --expect=enter --reverse --border'
--   local tmpfile = vim.fn.tempname()
--   local fzfcmd = string.format('fzf %s %s > %s', default_fzfopts, args.fzfopts, tmpfile)
--   local cmd = nil
--   if args.stdin then
--     local stdin = args.stdin()
--     if type(stdin) == 'table' then
--       cmd = string.format("cat << 'EOF' | %s\n%s\nEOF\n", fzfcmd, vim.fn.join(stdin, '\n'))
--     else
--       cmd = string.format("cat << EOF | %s\n$(%s)\nEOF\n", fzfcmd, stdin)
--     end
--   else
--     cmd = fzfcmd
--   end
--
--   -- print(cmd)
--   local jobid = vim.fn.jobstart(cmd, {
--     term = true,
--     on_exit = function(_, exit_code)
--       pcall(vim.api.nvim_win_close, win, true)
--       pcall(vim.api.nvim_buf_delete, buf, { force = true })
--
--       local ok, result = pcall(vim.fn.readfile, tmpfile)
--       vim.fn.delete(tmpfile)
--       if not ok then
--         error(vim.inspect(result))
--       end
--
--       if exit_code > 128 then
--         return
--       end
--
--       local query = result[1]
--       local key = result[2]
--       local selections = vim.list_slice(result, 3)
--       if args.handlers[key] then
--         args.handlers[key](selections)
--       end
--     end,
--   })
--
--   vim.cmd 'startinsert'
-- end
--
-- local fzf_files = function()
--   fzf_run {
--     window = { title = 'Files' },
--     stdin = function()
--       local input = 'fd --type file --unrestricted'
--       for _, p in ipairs(vim.opt.path:get()) do
--         if p ~= "" then
--           input = input .. ' --search-path ' .. p
--         end
--       end
--       return input
--     end,
--     fzfopts = "--multi --border-label=Files",
--     handlers = {
--       enter = function(selections)
--         for _, s in ipairs(selections) do
--           vim.cmd("edit " .. s)
--         end
--       end
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader><space>', fzf_files)
--
-- local fzf_diagnostics = function()
--   fzf_run {
--     window = { title = 'Diagnostics' },
--     stdin = function()
--       local result = {}
--       local diagnostics = vim.fn.sort(vim.diagnostic.get(), function(a, b) return a.severity - b.severity end)
--       for _, d in ipairs(diagnostics) do
--         table.insert(result, d.message)
--       end
--       return result
--     end,
--     handlers = {
--       enter = function(selections)
--         print(vim.inspect(selections))
--       end,
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader>fd', fzf_diagnostics)
--
-- local fzf_buffers = function()
--   local insert_bufname = function(bufnames, bufnr)
--     local ok, bufname = pcall(vim.api.nvim_buf_get_name, bufnr)
--     if ok and vim.fn.buflisted(bufnr) ~= 0 and vim.fn.bufloaded(bufnr) ~= 0 and bufname ~= "" then
--       table.insert(bufnames, bufname)
--     end
--   end
--
--   local current = vim.api.nvim_get_current_buf()
--   local alt = vim.fn.bufnr('#')
--
--   fzf_run {
--     window = { title = 'Buffers' },
--     stdin = function()
--       local bufnames = {}
--       insert_bufname(bufnames, current)
--       insert_bufname(bufnames, alt)
--       for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
--         if bufnr ~= current and bufnr ~= alt then
--           insert_bufname(bufnames, bufnr)
--         end
--       end
--       return bufnames
--     end,
--     fzfopts = '--header-lines=1 --border-label=Buffers',
--     handlers = {
--       enter = function(selections)
--         vim.cmd("b " .. selections[1])
--       end,
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader>,', fzf_buffers)
--
-- local fzf_oldfiles = function()
--   fzf_run {
--     stdin = function()
--       return vim.v.oldfiles
--     end,
--     fzfopts = '--sync --border-label=Oldfiles',
--     handlers = {
--       enter = function(selections)
--         for _, s in ipairs(selections) do
--           vim.cmd("e " .. s)
--         end
--       end,
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader>fo', fzf_oldfiles)
--
-- local fzf_history = function()
--   fzf_run {
--     window = { title = 'History' },
--     stdin = function()
--       local history = {}
--       for line in vim.fn.execute('history'):gmatch('\n>? +%d+ +([^\n]+)\n?') do
--         table.insert(history, line)
--       end
--       return history
--     end,
--     fzfopts = '--tac',
--     handlers = {
--       enter = function(selections)
--         vim.cmd(selections[1])
--       end,
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader>fh', fzf_history)
--
-- local fzf_grep = function()
--   fzf_run {
--     window = { title = 'Grep' },
--     fzfopts = function()
--       local fzfopts = [[--ansi --disabled --bind 'start:reload:true']]
--       fzfopts = fzfopts .. [[ --bind 'change:transform:echo "reload:sleep 0.1; q={q}; if [ \"\${q/ --/}\" = \"\$q\" ]; then rg --vimgrep --color=always -e \"\$q\" 2>&1; else eval \"rg --vimgrep --color=always -e '\''\${q%% --*}'\'' \${q#* --} || true\"; fi"']]
--       return fzfopts
--     end,
--     handlers = {
--       enter = function(selections)
--       end,
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader>fg', fzf_grep)
--
-- local fzf_lines = function()
--   local bufnr = vim.fn.bufnr()
--   fzf_run {
--     window = { title = 'Lines' },
--     stdin = function()
--       return vim.api.nvim_buf_get_lines(bufnr, 0, -1, true)
--     end,
--     handlers = {
--       enter = function(selections)
--       end,
--     }
--   }
-- end
--
-- vim.keymap.set('n', '<leader>/', fzf_lines)
--
-- -- lines
--
-- -- resume
--
-- return {}
