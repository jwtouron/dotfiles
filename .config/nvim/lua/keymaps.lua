local set = vim.keymap.set

set('n', 'n', [[v:searchforward ? 'nzzzv' : 'Nzzzv']], { expr = true })
set('n', 'N', [[v:searchforward ? 'Nzzzv' : 'nzzzv']], { expr = true })

set({'n', 'x'}, 'cg*', '*Ncgn')


-- From Primeagen

set("v", "J", ":m '>+1<CR>gv=gv")
set("v", "K", ":m '<-2<CR>gv=gv")

set("n", "J", "mzJ`z")
set("n", "<C-d>", "<C-d>zz")
set("n", "<C-u>", "<C-u>zz")
-- set("n", "n", "nzzzv")
-- set("n", "N", "Nzzzv")

-- greatest remap ever
set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
set({"n", "v"}, "<leader>y", [["+y]])
set("n", "<leader>Y", [["+Y]])

set({"n", "v"}, "<leader>d", [["_d]])

-- This is going to get me cancelled
set("i", "<C-c>", "<Esc>")


-- From mini.basics

set({ 'n', 'x' }, 'j', [[v:count == 0 ? 'gj' : 'j']], { expr = true })
set({ 'n', 'x' }, 'k', [[v:count == 0 ? 'gk' : 'k']], { expr = true })

-- Add empty lines before and after cursor line
set('n', 'gO', "<Cmd>call append(line('.') - 1, repeat([''], v:count1))<CR>", { desc = 'Put empty line above' })
set('n', 'go', "<Cmd>call append(line('.'),     repeat([''], v:count1))<CR>", { desc = 'Put empty line below' })

-- Copy/paste with system clipboard
set({ 'n', 'x' }, 'gy', '"+y', { desc = 'Copy to system clipboard' })
set(  'n',        'gp', '"+p', { desc = 'Paste from system clipboard' })
-- - Paste in Visual with `P` to not copy selected text (`:h v_P`)
set(  'x',        'gp', '"+P', { desc = 'Paste from system clipboard' })

-- Reselect latest changed, put, or yanked text
set('n', 'gV', '"`[" . strpart(getregtype(), 0, 1) . "`]"', { expr = true, desc = 'Visually select changed text' })

-- Search inside visually highlighted text. Use `silent = false` for it to
-- make effect immediately.
set('x', 'g/', '<esc>/\\%V', { silent = false, desc = 'Search inside visual selection' })

-- Search visually selected text (slightly better than builtins in Neovim>=0.8)
set('x', '*', [[y/\V<C-R>=escape(@", '/\')<CR><CR>]])
set('x', '#', [[y?\V<C-R>=escape(@", '?\')<CR><CR>]])

-- Correct latest misspelled word by taking first suggestion.
-- Use `<C-g>u` in Insert mode to mark this as separate undoable action.
-- Source: https://stackoverflow.com/a/16481737
-- NOTE: this remaps `<C-z>` in Normal mode (completely stops Neovim), but
-- it seems to be too harmful anyway.
set('n', '<C-Z>', '[s1z=',                     { desc = 'Correct latest misspelled word' })
set('i', '<C-Z>', '<C-g>u<Esc>[s1z=`]a<C-g>u', { desc = 'Correct latest misspelled word' })

set('n', '<leader>oc', '<Cmd>setlocal cursorline! cursorline?<CR>')
set('n', '<leader>oC', '<Cmd>setlocal cursorcolumn! cursorcolumn?<CR>')
set('n', '<leader>oh', '<Cmd>let v:hlsearch = 1 - v:hlsearch | echo (v:hlsearch ? "  " : "no") . "hlsearch"<CR>')
set('n', '<leader>oi', '<Cmd>setlocal ignorecase! ignorecase?<CR>')
set('n', '<leader>ol', '<Cmd>setlocal list! list?<CR>')
set('n', '<leader>on', '<Cmd>setlocal number! number?<CR>')
set('n', '<leader>or', '<Cmd>setlocal relativenumber! relativenumber?<CR>')
set('n', '<leader>os', '<Cmd>setlocal spell! spell?<CR>')
set('n', '<leader>ow', '<Cmd>setlocal wrap! wrap?<CR>') 
