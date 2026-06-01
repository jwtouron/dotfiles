-- Delete trailing whitespace in insert mode on <cr>
vim.keymap.set("i",
  "<cr>", [[ getline('.')[col('.') -2] =~# '\s' ? "<esc>ciw<cr>" : "<cr>" ]],
  { expr = true, desc = "Clear whitespace at end of line on enter" }
)

-- Select last pasted text
vim.keymap.set("n", "gp", "`[v`]", { silent = true })

-- Easy repeat editing
vim.keymap.set("n", "cg*", "*Ncgn", { desc = "Change word under cursor, '.' to continue." })
vim.keymap.set("x", "c", function()
  if vim.fn.mode() == 'v' then
    vim.cmd 'norm! y'
    vim.cmd 'let @/=@"'
    vim.fn.feedkeys("cgn", "n")
  else
    vim.fn.feedkeys("c", "n")
  end
end, { desc = "Change visual selection, '.' to continue." })

-- Copy lines and comment out

local ycc_count = 1

_G._ycc_operator_func = function()
  local count = ycc_count
  vim.cmd("normal! " .. count .. "yy")
  vim.cmd("normal! P")
  vim.cmd("normal! " .. count .. "j")
  vim.cmd("normal " .. count .. "gcc")
  vim.cmd("normal! " .. count .. "k")
end

vim.keymap.set("n", "ycc", function()
  ycc_count = vim.v.count1
  vim.o.operatorfunc = "v:lua._ycc_operator_func"
  vim.fn['repeat#set']("ycc", ycc_count)
  vim.cmd "normal! g@l"
end, { desc = "Duplicate and comment out line." })

vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

vim.keymap.set('t', '<esc>', [[<C-\><C-n>]])
vim.keymap.set('t', '<C-w>', [[<C-\><C-n><C-w>]])

-- Resize window using <ctrl> arrow keys
vim.keymap.set("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- In cmdline, <tab> will either "accept" then entry if wildmode is active, otherwise act as <tab>
-- vim.keymap.set("c", "<Tab>", function()
--   if vim.fn.wildmenumode() ~= 0 then
--     vim.cmd.call 'feedkeys("\\<C-y>")'
--   else
--     vim.fn.wildtrigger()
--   end
-- end)

-- Navigation
vim.keymap.set("n", "<leader><space>", ":find ")
vim.keymap.set("n", "<leader>,", ":b ")

vim.keymap.set("c", "/", function()
  if vim.fn.getcmdline():sub(vim.fn.getcmdpos() - 1) == "/"
    and vim.fn.wildmenumode() ~= 0
  then
    return "<C-y>"
  else
    return "/"
  end
end, { expr = true })
