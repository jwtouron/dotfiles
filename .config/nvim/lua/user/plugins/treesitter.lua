-- Not sure why this doesn't work when placed under the 'config' key of playground...
CreateFileTypeAutocmd("tsplayground", function()
  vim.keymap.set("n", "q", "<cmd>q<cr>", { buffer = true, silent = true, })
end)

return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    build = ":TSUpdate",
    config = function()
      require'nvim-treesitter.configs'.setup {
        -- These five should always be installed: "c", "lua", "vim", "vimdoc", "query"
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline", },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
      }
    end
  },

  "nvim-treesitter/playground",
  dependencies = "nvim-treesitter/nvim-treesitter",
  cmd = {
    "TSHighlightCapturesUnderCursor",
    "TSNodeUnderCursor",
    "TSPlaygroundToggle",
  },
}
