vim.pack.add(
  { "https://github.com/nvim-tree/nvim-web-devicons", "https://github.com/ibhagwan/fzf-lua" },
  { confirm = false }
)

local setup
setup = function()
  require('fzf-lua').setup {
    buffers = {
      file_icons = false,
    },
    files = {
      no_ignore = true,
      git_icons = false,
      file_icons = false,
    },
    grep = {
      file_icons = false,
      query_delay = 300,
      rg_glob = true,
      -- first returned string is the new search query
      -- second returned string are (optional) additional rg flags
      -- @return string, string?
      rg_glob_fn = function(query)
        local regex, flags = query:match("^(.-)%s%-%-(.*)$")
        -- If no separator is detected will return the original query
        return (regex or query), flags
      end
    },
    winopts = {
      on_create = function()
        local opts = { nowait = true, buffer = true }
        vim.keymap.set("t", "<C-b>", "<Left>", opts)
        vim.keymap.set("t", "<C-f>", "<Right>", opts)
      end,
      preview = { hidden = true },
    },
  }
  setup = function() end
end

vim.ui.select = function(items, opts, on_choice)
  setup()
  require('fzf-lua').register_ui_select()
  vim.ui.select(items, opts, on_choice)
end

for _, cmd in ipairs({ { "Files", "files" }, { "LiveGrep", "live_grep", } }) do
  vim.api.nvim_create_user_command('FzfLua' .. cmd[1], function(arg)
    local dir = arg.args
    if dir == "" then
      dir = "."
    end
    require('fzf-lua')[cmd[2]] { cwd = dir }
  end,
  {
    complete = 'dir',
    nargs = '?',
  })
end

local function keymap_set(mode, lhs, rhs, opts)
  local rhs2
  if type(rhs) == 'string' then
    opts = opts or {}
    opts.expr = true
    rhs2 = function() setup(); return rhs end
  else
    rhs2 = function() setup(); rhs() end
  end
  vim.keymap.set(mode, lhs, rhs2, opts)
end

keymap_set('n', "<leader><space>", function() require('fzf-lua').files() end, { desc = "FZF Files" })
keymap_set('n', "<leader>,", function() require('fzf-lua').buffers() end, { desc = "FZF Buffers" })
keymap_set('n', "<leader>/", function() require('fzf-lua').blines() end, { desc = "FZF Buffer Lines" })
keymap_set('n', "<leader>]", function() require('fzf-lua').tags() end, { desc = "FZF Tags" })

keymap_set('n', "<leader>zc", function() require('fzf-lua').command_history() end, { desc = "FZF Command History" })
keymap_set('n', "<leader>zd", function() require('fzf-lua').diagnostics_document() end, { desc = "FZF Diagnostics Document" })
keymap_set('n', "<leader>zD", function() require('fzf-lua').diagnostics_workspace() end, { desc = "FZF Diagnostics Workspace" })
keymap_set('n', "<leader>zf", function() require('fzf-lua').files() end, { desc = "FZF Files" })
keymap_set('n', "<leader>zF", ":FzfLuaFiles ", { desc = "FZF Files (custom dir)" })
keymap_set('n', "<leader>zg", function() require('fzf-lua').live_grep() end, { desc = "FZF Live Grep" })
keymap_set('n', "<leader>zG", ":FzfLuaLiveGrep ", { desc = "FZF Live Grep (custom dir)" })
keymap_set('n', "<leader>zh", function() require('fzf-lua').helptags() end, { desc = "FZF Help Tags" })
keymap_set('n', "<leader>zk", function() require('fzf-lua').keymaps() end, { desc = "FZF Keymaps" })
keymap_set('n', "<leader>zl", function() require('fzf-lua').loclist() end, { desc = "FZF Loclist" })
keymap_set('n', "<leader>zm", function() require('fzf-lua').marks() end, { desc = "FZF Marks" })
keymap_set('n', "<leader>zM", function() require('fzf-lua').manpages() end, { desc = "FZF Man Pages" })
keymap_set('n', "<leader>zo", function() require('fzf-lua').oldfiles() end, { desc = "FZF Old Files" })
keymap_set('n', "<leader>zq", function() require('fzf-lua').quickfix() end, { desc = "FZF Quickfix" })
keymap_set('n', "<leader>zr", function() require('fzf-lua').registers() end, { desc = "FZF Registers" })
keymap_set('n', "<leader>zR", function() require('fzf-lua').resume() end, { desc = "FZF Resume" })
keymap_set('n', "<leader>zs", function() require('fzf-lua').spell_suggest() end, { desc = "FZF Spell Suggest" })
keymap_set('n', "<leader>zz", function() require('fzf-lua').builtin() end, { desc = "FZF Builtin" })

keymap_set('n', "<leader>lc", function() require('fzf-lua').lsp_code_actions() end, { desc = "FZF LSP Code Actions" })


-- local function fzf(args)
--   local fzfcmd = 'fzf --style=full --print-query --expect=enter,ctrl-t'
--
--   if type(args.stdin) == 'function' then
--     args.stdin = args.stdin()
--   end
--
--   if type(args.stdin) == 'table' then
--     args.stdin = "printf " .. vim.fn.shellescape(vim.fn.join(args.stdin, '\\0'))
--     fzfcmd = fzfcmd .. ' --read0'
--   end
--
--   local buf = vim.api.nvim_create_buf(false, true)
--   local win = vim.api.nvim_open_win(buf, true, {
--     row = 5,
--     col = 5,
--     height = vim.o.lines - 5 * 2 - 2,
--     width = vim.o.columns - 5 * 2 - 2,
--     style = 'minimal',
--     border = 'none',
--     relative = 'editor',
--   })
--
--   local tempname = vim.fn.tempname()
--   vim.fn.jobstart(string.format("cat << EOF | %s > %s\n$(%s)", fzfcmd, tempname, args.stdin), {
--     term = true,
--     on_exit = function()
--       vim.schedule(function()
--         if vim.api.nvim_win_is_valid(win) then
--           vim.api.nvim_win_close(win, true)
--         end
--         if vim.api.nvim_buf_is_valid(buf) then
--           vim.api.nvim_buf_delete(buf, { force = true })
--         end
--       end)
--       vim.uv.fs_stat(tempname, function(err)
--         if err == nil then
--           vim.schedule(function()
--             print(vim.inspect(vim.fn.readfile(tempname)))
--             os.remove(tempname)
--           end)
--         else
--           error(err)
--         end
--       end)
--     end
--   })
--   vim.cmd 'startinsert'
-- end

-- fzf {
--   stdin = {"a", "b"},
--   -- stdin = "fd --type file --unrestricted --exclude='.git'"
-- }

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

