return {
  {
    "nvim-orgmode/orgmode",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("orgmode").setup_ts_grammar()
      require("nvim-treesitter.configs").setup {
        ensure_installed = { "org" },
        highlight = {
          additional_vim_regex_highlighting = { "org" },
        },
      }

      require("orgmode").setup {
        org_agenda_files = { "~/Documents/notes/agenda/*", },
      }
    end,
  },
}
