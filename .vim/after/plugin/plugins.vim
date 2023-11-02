vim9script

# airblade/vim-rooter
g:rooter_change_directory_for_non_project_files = 'current'
g:rooter_patterns = g:rooter_patterns + [".rooter", "=" .. $HOME .. "/.config/nvim"]

# junegunn/fzf.vim
g:fzf_vim = {preview_window: ['up,50%', 'alt-/']}

# machakann/vim-highlightedyank
g:highlightedyank_highlight_duration = 250
g:highlightedyank_highlight_in_visual = 0
highlight HighlightedyankRegion cterm=reverse gui=reverse

# moll/vim-bbye
cabbrev bd Bdelete
cabbrev bw Bwipeout

# romainl/vim-cool
g:cool_total_matches = 1

# romainl/vim-qf
g:qf_mapping_ack_style = 1
g:qf_auto_resize = 0
g:qf_max_height = 0

# yegappan/lsp
var lspOpts = {
  autoComplete: v:false,
  diagSignErrorText: "󰅚 ",
  diagSignHintText: "󰌶 ",
  diagSignInfoText: " ",
  diagSignWarningText: " "
}

def MyLspAttached()
  nnoremap <buffer> [D :LspDiagFirst<cr>
  nnoremap <buffer> ]D :LspDiagLast<cr>
  nnoremap <buffer> ]d :LspDiagNext<cr>
  nnoremap <buffer> [d :LspDiagPrev<cr>
  nnoremap <buffer> gl :LspDiagCurrent<cr>
  nnoremap <buffer> gd :LspGotoDefinition<cr>
  nnoremap <buffer> K :LspHover<cr>
  setlocal completeopt=menu,menuone,noinsert,noselect
enddef

autocmd myaugroup User LspAttached MyLspAttached()
# autocmd myaugroup VimEnter * g:LspOptionsSet(lspOpts)
g:LspOptionsSet(lspOpts)
