--------------------------------------------------------------------------------
-- Buffers
--------------------------------------------------------------------------------

vim.keymap.set("n", "<leader>bb", "<cmd>e #<cr>", { desc = "Switch to Other Buffer" })

--------------------------------------------------------------------------------
-- Cursor Movement
--------------------------------------------------------------------------------

-- Move by visual lines.
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- When searching 'n' is always down, 'N' is always up.
-- Center after jumping
vim.keymap.set("n", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true, desc = "Next search result" })
vim.keymap.set("x", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true, desc = "Next search result" })
vim.keymap.set("o", "n", [[v:searchforward?'n':'N']], { expr = true, silent = true, desc = "Next search result" })
vim.keymap.set("n", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true, desc = "Prev search result" })
vim.keymap.set("x", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true, desc = "Prev search result" })
vim.keymap.set("o", "N", [[v:searchforward?'N':'n']], { expr = true, silent = true, desc = "Prev search result" })

-- Center after moving.
vim.keymap.set("n", "<C-d>", "<C-d>zz", { silent = true, desc = { "Move down half screen." } })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { silent = true, desc = { "Move up half screen." } })
--
vim.keymap.set("n", "<C-f>", "<C-f>zz", { silent = true, desc = { "Move down full screen." } })
vim.keymap.set("n", "<C-b>", "<C-b>zz", { silent = true, desc = { "Move up full screen." } })

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- Don't move cursor when joining lines (uses 'z' mark)
vim.keymap.set("n", "J", "mzJ`z", { silent = true, desc = "Join lines" })

-- Execute macro saved at 'q'
vim.keymap.set("n", "Q", "@q", { desc = "Run macro 'q'" })

-- Easy editing of words (cg* on a word, edit, then '.' or 'n')
vim.keymap.set("n", "cg*", "*Ncgn", { desc = "Change word under cursor, '.' to continue." })

-- Don't overwrite paste register when pasting in visual mode
vim.keymap.set("x", "p", [["_dP]], { desc = "Paste in visual mode without overwriting paste register." })

-- Add undo break-points
vim.keymap.set("i", ",", ",<c-g>u")
vim.keymap.set("i", ".", ".<c-g>u")
vim.keymap.set("i", ";", ";<c-g>u")

--------------------------------------------------------------------------------
-- Tabs
--------------------------------------------------------------------------------

vim.keymap.set("n", "<leader><tab>l", "<cmd>tablast<cr>", { desc = "Last Tab" })
vim.keymap.set("n", "<leader><tab>f", "<cmd>tabfirst<cr>", { desc = "First Tab" })
vim.keymap.set("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", { desc = "New Tab" })
vim.keymap.set("n", "<leader><tab>]", "<cmd>tabnext<cr>", { desc = "Next Tab" })
vim.keymap.set("n", "<leader><tab>d", "<cmd>tabclose<cr>", { desc = "Close Tab" })
vim.keymap.set("n", "<leader><tab>[", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })

--------------------------------------------------------------------------------
-- Toggle UI options
--------------------------------------------------------------------------------

local toggle_prefix = "<leader>u"

local function toggle(key, options)
  if type(options) ~= "table" then
    options = { options }
  end
  local desc = "Toggle "..options[1]
  vim.keymap.set("n", toggle_prefix..key, function()
    local oldval = vim.opt_local[options[1]]:get()
    for _, option in ipairs(options) do
      if oldval then
        vim.opt_local[option] = false
      else
        vim.opt_local[option] = true
      end
    end
  end, { desc = desc })
end

toggle("l", "cursorline")
toggle("n", { "number", "relativenumber" })
toggle("s", "spell")
toggle("w", "wrap")

--------------------------------------------------------------------------------
-- Windows
--------------------------------------------------------------------------------

-- Move to window using the <ctrl> hjkl keys
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Go to left window" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Go to lower window" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Go to upper window" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Go to right window" })

-- Resize window using <ctrl> arrow keys
vim.keymap.set("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- Creation/Deletion
vim.keymap.set("n", "<leader>ww", "<C-W>p", { desc = "Other window" })
vim.keymap.set("n", "<leader>wd", "<C-W>c", { desc = "Delete window" })
vim.keymap.set("n", "<leader>w-", "<C-W>s", { desc = "Split window horizontally" })
vim.keymap.set("n", "<leader>w|", "<C-W>v", { desc = "Split window vertically" })
vim.keymap.set("n", "<leader>-", "<C-W>s", { desc = "Split window horizontally" })
vim.keymap.set("n", "<leader>|", "<C-W>v", { desc = "Split window vertically" })
