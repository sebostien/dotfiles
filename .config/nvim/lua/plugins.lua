-------------------------------------------------
-------------------- Plugins --------------------
-------------------------------------------------
local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP =
		fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({
				border = "rounded",
			})
		end,
	},
})

return packer.startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")

	---- Theme ----
	use("EdenEast/nightfox.nvim")

	-- Dependencies
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use("nvim-lua/plenary.nvim") -- Useful lua functions used by lots of plugins

	---- Other ----
	use("elkowar/yuck.vim") -- Eww widgets // TODO
	use("feline-nvim/feline.nvim")
	use("folke/which-key.nvim") -- // TODO

	-- Breadcrumbs

	-- Indentation Guides
	use({
		"lukas-reineke/indent-blankline.nvim",
		config = function()
			--vim.opt.list = true
			require("indent_blankline").setup({})
		end,
	})

	-- // TODO
	-- Telescope
	use({
		"nvim-telescope/telescope.nvim",
		requires = { { "nvim-lua/plenary.nvim" } },
	})

	-- Neotree
	use({
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v2.x",
		requires = {
			"nvim-lua/plenary.nvim",
			"kyazdani42/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		},
		config = function()
			vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
		end,
	})

	-- Code outline
	use({
		"stevearc/aerial.nvim",
		config = function()
			require("aerial").setup()
		end,
	})

	-- Treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	})

	-- Comments
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})
	use("JoosepAlviste/nvim-ts-context-commentstring") -- Detect embedded language and comment correctly

	-- LSP
	use({
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"neovim/nvim-lspconfig",
		"jose-elias-alvarez/null-ls.nvim", -- Formatters and Linters
	})

	-- Autopairs
	use({
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	})

	-- JSON SchemaStore
	use("b0o/schemastore.nvim")

	-- // TODO:
	-- Completion
	use({
		"ms-jpq/coq_nvim",
		branch = "coq",
	})
	use("ms-jpq/coq.artifacts") -- Snippets

	-- Git decorations
	use({
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup({
				signs = {
					untracked = { text = " " },
				},
			})
		end,
	})

	-- Sync after packer update/install
	-- Keep last
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
