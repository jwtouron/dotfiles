-- To use, start Neovim with:
-- nvim --cmd 'let g:load_startuptime = 1'
vim.pack.add(
  { "https://github.com/dstein64/vim-startuptime" },
  {
    confirm = false,
    load = function(plugin)
      if vim.g.load_startuptime ~= 0 and vim.g.load_startuptime then
        vim.cmd.packadd(plugin.spec.name)
        vim.cmd "StartupTime"
      end
    end,
  }
)
