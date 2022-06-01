-------------------------------------------------
-------------------- Plugins --------------------
-------------------------------------------------
local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = fn.system {'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim',
                                  install_path}
    print 'Installing packer close and reopen Neovim...'
    vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, 'packer')
if not status_ok then
    return
end

-- Have packer use a popup window
packer.init {
    display = {
        open_fn = function()
            return require('packer.util').float {
                border = 'rounded'
            }
        end
    }
}

return packer.startup(function(use)

    use 'wbthomason/packer.nvim'

    ---- Theme ----
    use 'EdenEast/nightfox.nvim'

    ---- Other ----
    use 'feline-nvim/feline.nvim'
    use 'folke/which-key.nvim'
    use 'elkowar/yuck.vim' -- Eww widgets
    use 'nvim-lua/popup.nvim' -- An implementation of the Popup API from vim in Neovim
    use 'nvim-lua/plenary.nvim' -- Useful lua functions used ny lots of plugins
    use 'windwp/nvim-autopairs' -- Autopairs, integrates with both cmp and treesitter

    -- Telescope
    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/plenary.nvim'}}
    }

    -- Neotree
    use {
        'nvim-neo-tree/neo-tree.nvim',
        branch = 'v2.x',
        requires = {'nvim-lua/plenary.nvim', 'kyazdani42/nvim-web-devicons', -- not strictly required, but recommended
        'MunifTanjim/nui.nvim'}
    }

    -- Treesitter
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use 'JoosepAlviste/nvim-ts-context-commentstring'

    -- LSP
    use 'neovim/nvim-lspconfig' -- enable LSP
    use 'williamboman/nvim-lsp-installer' -- simple to use language server installer
    use 'tamago324/nlsp-settings.nvim' -- language server settings defined in json for
    use 'jose-elias-alvarez/null-ls.nvim' -- for formatters and linters

    -- cmp plugins
    use 'hrsh7th/nvim-cmp' -- The completion plugin
    use 'hrsh7th/cmp-buffer' -- buffer completions
    use 'hrsh7th/cmp-path' -- path completions
    use 'hrsh7th/cmp-cmdline' -- cmdline completions
    use 'saadparwaiz1/cmp_luasnip' -- snippet completions
    use 'hrsh7th/cmp-nvim-lsp'

    -- Git
    use 'lewis6991/gitsigns.nvim'

    -- Sync after packer update/install
    -- Keep last
    if PACKER_BOOTSTRAP then
        require('packer').sync()
    end
end)
