local function live_grep()
  require('fzf-lua').fzf_live(function(query)
    local pattern = nil
    local opts = nil
    local idx = string.find(query, ' --')
    if idx then
      pattern = vim.fn.shellescape(string.sub(query, 1, idx - 1))
      opts = ' ' .. string.sub(query, idx + 3)
    else
      pattern = vim.fn.shellescape(string.sub(query, 1))
      opts = ''
    end
    return 'rg --vimgrep -e ' .. pattern .. opts
  end,
  {
    actions = {
      ["default"] = require('fzf-lua').actions.file_edit_or_qf,
      ["ctrl-s"]  = require('fzf-lua').actions.file_split,
      ["ctrl-v"]  = require('fzf-lua').actions.file_vsplit,
      ["ctrl-t"]  = require('fzf-lua').actions.file_tabedit,
      ["alt-q"]   = require('fzf-lua').actions.file_sel_to_qf,
      ["alt-l"]   = require('fzf-lua').actions.file_sel_to_ll,
    },
    fzf_opts = {
      ['--preview'] = 'if command -v bat >/dev/null; then bat -p --color always --highlight-line {2} {1}; else cat {1}; fi',
      ['--preview-window'] = 'nohidden,down,50%,+{2}/3',
      ['--delimiter'] = ':',
      ['--multi'] = '',
    },
  })
end

return {
  "ibhagwan/fzf-lua",
  dependencies = "nvim-tree/nvim-web-devicons",
  keys = {
    { "<leader><space>", "<cmd>lua require('fzf-lua').files()<cr>", desc = "FZF Files" },
    { "<leader>,", "<cmd>lua require('fzf-lua').buffers()<cr>", desc = "FZF Buffers" },
    { "<leader>/", "<cmd>lua require('fzf-lua').blines()<cr>", desc = "FZF Buffer Lines" },

    { "<leader>zc", "<cmd>lua require('fzf-lua').command_history()<cr>", desc = "FZF Command History" },
    { "<leader>zd", "<cmd>lua require('fzf-lua').diagnostics_document()<cr>", desc = "FZF Diagnostics Document" },
    { "<leader>zD", "<cmd>lua require('fzf-lua').diagnostics_workspace()<cr>", desc = "FZF Diagnostics Workspace" },
    { "<leader>zf", "<cmd>lua require('fzf-lua').files()<cr>", desc = "FZF Files" },
    { "<leader>zF", ":FzfLua files cwd=", desc = "FZF Files (specify cwd)" },
    { "<leader>zg", "<cmd>FzfLua live_grep_glob<cr>", desc = "FZF Live Grep Glob" },
    { "<leader>zG", ":FzfLua live_grep_glob cwd=", desc = "FZF Live Grep Glob (specify cwd)" },
    { "<leader>zh", "<cmd>lua require('fzf-lua').helptags()<cr>", desc = "FZF Help Tags" },
    { "<leader>zk", "<cmd>lua require('fzf-lua').keymaps()<cr>", desc = "FZF Keymaps" },
    { "<leader>zl", "<cmd>lua require('fzf-lua').loclist()<cr>", desc = "FZF Loclist" },
    { "<leader>zm", "<cmd>lua require('fzf-lua').marks()<cr>", desc = "FZF Marks" },
    { "<leader>zM", "<cmd>lua require('fzf-lua').manpages()<cr>", desc = "FZF Man Pages" },
    { "<leader>zo", "<cmd>lua require('fzf-lua').oldfiles()<cr>", desc = "FZF Old Files" },
    { "<leader>zq", "<cmd>lua require('fzf-lua').quickfix()<cr>", desc = "FZF Quickfix" },
    { "<leader>zr", "<cmd>lua require('fzf-lua').registers()<cr>", desc = "FZF Registers" },
    { "<leader>zR", "<cmd>lua require('fzf-lua').resume()<cr>", desc = "FZF Resume" },
    { "<leader>zs", "<cmd>lua require('fzf-lua').spell_suggest()<cr>", desc = "FZF Spell Suggest" },
    { "<leader>zz", "<cmd>lua require('fzf-lua').builtin()<cr>", desc = "FZF Builtin" },

    { "<leader>lc", "<cmd>lua require('fzf-lua').lsp_code_actions()<cr>", desc = "FZF LSP Code Actions" },
  },
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
