return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {},
  keys = {
    { "<leader><space>", "<cmd>FzfLua files<cr>", desc = "Search for files with FZF" },
    { "<leader>,", "<cmd>FzfLua buffers<cr>", desc = "Search for open buffers with FZF" },
    { "<leader>/", "<cmd>FzfLua blines<cr>", desc = "Search buffer lines with FZF" },
    { "<leader>]", "<cmd>FzfLua tags<cr>", desc = "Search tags with FZF" },

    { "<leader>zc", "<cmd>FzfLua commands<cr>", desc = "Search commands with FZF" },
    { "<leader>zg", "<cmd>FzfLua live_grep_glob<cr>", desc = "Live grep with FZF" },
    { "<leader>zh", "<cmd>FzfLua help_tags<cr>", desc = "Search help tags with FZF" },
    { "<leader>zk", "<cmd>FzfLua keymaps<cr>", desc = "Search keymaps with FZF" },
    { "<leader>zl", "<cmd>FzfLua loclist<cr>", desc = "Search loclist with FZF" },
    { "<leader>zm", "<cmd>FzfLua marks<cr>", desc = "Search marks with FZF" },
    { "<leader>zM", "<cmd>FzfLua man_pages<cr>", desc = "Search man pages with FZF" },
    { "<leader>zo", "<cmd>FzfLua oldfiles<cr>", desc = "Search oldfiles with FZF" },
    { "<leader>zq", "<cmd>FzfLua quickfix<cr>", desc = "Search quickfix with FZF" },
    { "<leader>zr", "<cmd>FzfLua registers<cr>", desc = "Search registers with FZF" },
    { "<leader>zR", "<cmd>FzfLua resume<cr>", desc = "Resume last FZF search" },
  },
}
