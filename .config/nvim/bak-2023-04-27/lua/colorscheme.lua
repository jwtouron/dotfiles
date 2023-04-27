vim.api.nvim_create_autocmd('ColorScheme', {
  group = vim.api.nvim_create_augroup('mycolorscheme', { clear = true }),
  callback = function() 
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    vim.cmd.highlight { 'MatchParen', 'term=underline', 'cterm=underline', 'gui=underline', 'ctermbg=NONE', 'guibg=NONE' }
  end,
})
