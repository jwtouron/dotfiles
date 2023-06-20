return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    build = ":TSUpdate",
    config = function()
      require'nvim-treesitter.configs'.setup {
        -- These five should always be installed: "c", "lua", "vim", "vimdoc", "query"
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
      }
    end
  },
}
