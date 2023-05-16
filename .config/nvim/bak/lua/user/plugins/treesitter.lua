return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    build = ":TSUpdate",
    config = function()
      require'nvim-treesitter.configs'.setup {
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query" },
        sync_install = false,
        auto_install = true,

        highlight = {
          enable = true,
        },

        incremental_selection = {
          enable = true,
          -- - keymaps:
          --   - init_selection: in normal mode, start incremental selection.
          --     Defaults to `gnn`.
          --   - node_incremental: in visual mode, increment to the upper named parent.
          --     Defaults to `grn`.
          --   - scope_incremental: in visual mode, increment to the upper scope
          --     (as defined in `locals.scm`). Defaults to `grc`.
          --   - node_decremental: in visual mode, decrement to the previous named node.
          --     Defaults to `grm`.
        }
      }
      end
  },
}
