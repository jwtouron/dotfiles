local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
    augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

-- Have packer use a popup window
packer.init({
    display = {
        open_fn = function()
            return require("packer.util").float({ border = "rounded" })
        end,
    },
})

return packer.startup(function(use)
    -- Dependencies
    use({ 'kyazdani42/nvim-web-devicons', opt = true })
    use({ 'nvim-lua/plenary.nvim' })

    use({ "ahmedkhalf/project.nvim", config = function() require("project_nvim").setup() end })
    use({ 'bronson/vim-trailing-whitespace' })
    use({ 'lewis6991/impatient.nvim' }) -- speed up startup time
    use({ 'moll/vim-bbye' })
    use({ 'nelstrom/vim-visual-star-search' })
    use({ 'norcalli/nvim-colorizer.lua', config = require('colorizer').setup })
    use({ 'numToStr/Comment.nvim', config = function() require('Comment').setup() end })
    use({ 'nvim-lualine/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true }, config = function() require('lualine').setup() end })
    use { 'nvim-telescope/telescope.nvim', requires = { {'nvim-lua/plenary.nvim'} } }
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    use { 'romainl/vim-cool' }
    use { 'romainl/vim-qlist' }
    use { 'tommcdo/vim-exchange' }
    use { 'tpope/vim-rsi' }
    use { 'tpope/vim-surround' }
    use { 'tpope/vim-unimpaired' }
    use { 'tpope/vim-vinegar' }

    -- Completion (cmp)
    use { 'hrsh7th/cmp-buffer' }
    use { 'hrsh7th/cmp-cmdline' }
    use { 'hrsh7th/cmp-nvim-lsp' }
    use { 'hrsh7th/cmp-path' }
    use { 'hrsh7th/cmp-vsnip' }
    use { 'hrsh7th/cmp-nvim-lsp-signature-help' }
    use { 'hrsh7th/nvim-cmp' }

    -- Snippets
    use { 'hrsh7th/vim-vsnip' }
    use { 'rafamadriz/friendly-snippets' }

    -- LSP
    use { "folke/trouble.nvim", requires = "kyazdani42/nvim-web-devicons", config = function() require("trouble").setup { } end }
    use { 'neovim/nvim-lspconfig' }
    use { "williamboman/nvim-lsp-installer", config = function() require('nvim-lsp-installer').setup {} end }

    use { 'LunarVim/Colorschemes' }

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
