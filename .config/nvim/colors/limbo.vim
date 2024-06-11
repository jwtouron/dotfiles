" Vim color file 'limbo.vim'
set background=dark
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="limbo"

" General colors
hi Normal        guifg=#ccc guibg=#171717
hi Cursor        guifg=#171717 guibg=#ccc
hi LineNr        guifg=#555
hi Comment       guifg=#666
hi Constant      guifg=#ffa182
hi String        guifg=#82daff
hi Function      guifg=#C984FF
hi Type          guifg=#a2a2a2
hi Keyword       guifg=#a2a2a2
hi KeywordItalic guifg=#a2a2a2 gui=italic
hi Usual         guifg=#d0d0d0
hi BlockLevelVariables guifg=#fff
hi BlockLevelVariablesItalic guifg=#fff gui=italic
hi Bold          guifg=#fff gui=bold
hi AfricanViolet      guifg=#C984FF
hi AfricanVioletItalic guifg=#C984FF gui=italic
hi BurntSienna        guifg=#ffa182
hi StringSymbols      guifg=#82daff
hi URL                gui=underline

" Editor colors
hi Search            guifg=#171717 guibg=#C984FF
hi IncSearch         guifg=#171717 guibg=#82daff
hi Visual            guifg=#171717 guibg=#C984FF

" Gutter colors
hi GitGutterAdd      guifg=#171717 guibg=#73C991
hi GitGutterChange   guifg=#171717 guibg=#E2C08D
hi GitGutterDelete   guifg=#171717 guibg=#F88070

" Minimap colors
hi MiniMapSelection  guifg=#171717 guibg=#C984FF
hi MiniMapAdded      guifg=#171717 guibg=#73C991
hi MiniMapDeleted    guifg=#171717 guibg=#F88070
hi MiniMapModified   guifg=#171717 guibg=#E2C08D

" Token colors
hi Invalid   guifg=#FF5370
