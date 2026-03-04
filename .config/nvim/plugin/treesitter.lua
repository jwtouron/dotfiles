if true then return end

local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.pack.add(
  {
    { src = 'https://github.com/nvim-treesitter/nvim-treesitter', version = 'main', },
    { src = 'https://github.com/nvim-treesitter/nvim-treesitter-textobjects', version = 'main', },
    { src = 'https://github.com/MeanderingProgrammer/treesitter-modules.nvim' },
    { src = 'https://github.com/aaronik/treewalker.nvim', },
  },
  { confirm = false, load = function() end }
)

vim.cmd.packadd('nvim-treesitter')

local setup
setup = function()
  vim.cmd.packadd('nvim-treesitter-textobjects')
  vim.cmd.packadd('treesitter-modules.nvim')
  vim.cmd.packadd('treewalker.nvim')

  require("nvim-treesitter-textobjects").setup()

  require("treewalker").setup {
    highlight = false,
    jumplist = false,
  }

  setup = function() end
end

vim.api.nvim_create_autocmd('FileType', {
  group = augroup,
  callback = function(args)
    local ok, parser = pcall(vim.treesitter.get_parser, args.buf)
    if not ok or not parser then
      return
    end

    setup()

    -- Select
    local select = require("nvim-treesitter-textobjects.select")
    local set_keymap_select = function(rhs, query_string, query_group)
      query_group = query_group or "textobjects"
      vim.keymap.set({ "x", "o" }, rhs, function()
        select.select_textobject(query_string, query_group)
      end, { buffer = args.buf })
    end
    set_keymap_select("af", "@function.outer")
    set_keymap_select("if", "@function.inner")
    set_keymap_select("ac", "@class.outer")
    set_keymap_select("ic", "@class.inner")
    set_keymap_select("as", "@local.scope", "locals")

    local tm = require('treesitter-modules')
    vim.keymap.set('n', '<C-=>', tm.init_selection, { buffer = args.buf })
    vim.keymap.set('x', '<C-=>', tm.node_incremental, { buffer = args.buf })
    vim.keymap.set('x', '<C-->', tm.node_decremental, { buffer = args.buf })

    -- Move
    local move = require("nvim-treesitter-textobjects.move")
    local function set_keymap_move(where, rhs, query_string, query_group)
      query_group = query_group or "textobjects"
      vim.keymap.set({ "n", "x", "o" }, rhs, function()
        move["goto_"..where](query_string, query_group)
      end, { buffer = args.buf })
    end
    set_keymap_move('next_start', "]m", "@function.outer")
    set_keymap_move('next_start', "][", "@class.outer")
    set_keymap_move('next_end', "]M", "@function.outer")
    set_keymap_move('next_end', "]]", "@class.outer")
    set_keymap_move('previous_start', "[m", "@function.outer")
    set_keymap_move('previous_start', "[[", "@class.outer")
    set_keymap_move('previous_end', "[M", "@function.outer")
    set_keymap_move('previous_end', "[]", "@class.outer")
    set_keymap_move('next_start', "]s", "@local.scope", "locals")
    set_keymap_move('previous_start', "[s", "@local.scope", "locals")
    set_keymap_move('next_start', "]z", "@fold", "folds")
    set_keymap_move('previous_start', "[z", "@fold", "folds")

    -- Treewalker
    vim.keymap.set({ 'n', 'x' }, '<C-k>', '<cmd>Treewalker Up<cr>', { silent = true })
    vim.keymap.set({ 'n', 'x' }, '<C-j>', '<cmd>Treewalker Down<cr>', { silent = true })
    vim.keymap.set({ 'n', 'x' }, '<C-h>', '<cmd>Treewalker Left<cr>', { silent = true })
    vim.keymap.set({ 'n', 'x' }, '<C-l>', '<cmd>Treewalker Right<cr>', { silent = true })
  end,
})
