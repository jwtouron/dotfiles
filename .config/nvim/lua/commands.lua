-- Handled by mini.bufremove
-- vim.api.nvim_create_user_command('BD', 'b#|bd#', {})
-- vim.api.nvim_create_user_command('Bd', 'b#|bd#', {})
-- vim.api.nvim_create_user_command('BW', 'b#|bw#', {})
-- vim.api.nvim_create_user_command('Bw', 'b#|bw#', {})

vim.api.nvim_create_user_command('ReadDate', "read !date +'\\%Y-\\%m-\\%d'", {})
