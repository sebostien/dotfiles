-------------------------------------------------
-------------------- Plugins --------------------
-------------------------------------------------
local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({
    "git",
    "clone",
    "--depth",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
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
  use("elkowar/yuck.vim") -- Eww widgets
  use("feline-nvim/feline.nvim")
  use("folke/which-key.nvim")
  use({
    -- Render Markdown
    "iamcco/markdown-preview.nvim",
    run = "cd app && npm install",
    setup = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
    ft = { "markdown" },
  })

  -- Git
  use("tpope/vim-fugitive")

  -- Latex
  use("jakewvincent/texmagic.nvim")

  -- Indentation Guides
  use({
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({})
    end,
  })

  -- Telescope
  use({
    "nvim-telescope/telescope-fzf-native.nvim",
    run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
  })
  use({
    "nvim-telescope/telescope.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").setup({})

      require("telescope").load_extension("fzf")
    end,
  })

  -- Marks
  use({
    "ThePrimeagen/harpoon",
    config = function()
      require("harpoon").setup({
        excluded_filetypes = {
          "harpoon",
          "^neo.tree$",
        },
      })
    end,
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

  -- Treesitter
  use({
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  })

  -- Undotree
  use("mbbill/undotree")

  -- Comments
  use({
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  })
  use("JoosepAlviste/nvim-ts-context-commentstring") -- Detect embedded language and comment correctly

  -- Color picker/preview
  use({
    "uga-rosa/ccc.nvim",
    config = function()
      -- Enable true color
      vim.opt.termguicolors = true
    end,
  })

  -- LSP
  use({
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
    "jose-elias-alvarez/null-ls.nvim", -- LSP config helper
    "simrat39/rust-tools.nvim", -- Rust-analyzer
    "MrcJkb/haskell-tools.nvim", -- Haskell
    "mfussenegger/nvim-dap", -- Debugger
    "j-hui/fidget.nvim", -- Show load status
  })
  use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })

  -- Rust crates helper
  use({
    "saecki/crates.nvim",
    event = { "BufRead Cargo.toml" },
    requires = { "nvim-lua/plenary.nvim" },
    config = function()
      require("crates").setup()
    end,
  })

  -- Autopairs
  use({
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({})
    end,
  })

  -- Task Runner
  use("stevearc/overseer.nvim")

  -- JSON SchemaStore
  use("b0o/schemastore.nvim")

  -- Neodev
  use("folke/neodev.nvim")

  -- Completion
  use({
    "hrsh7th/nvim-cmp",
    requires = {
      -- CMP Sources
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-calc",
      "f3fora/cmp-spell",
      -- Formatting
      "onsails/lspkind.nvim",
      -- Snippets
      "quangnguyen30192/cmp-nvim-ultisnips",
      "SirVer/ultisnips",
    },
  })

  -- TODO highlight
  use({
    "folke/todo-comments.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    config = function()
      require("todo-comments").setup({})
    end,
  })
  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup({
        auto_preview = false,
      })
    end,
  })

  -- Highlight word under cursor
  use({
    "RRethy/vim-illuminate",
    config = function()
      local il = require("illuminate")
      il.configure({
        under_cursor = false,
        min_count_to_highlight = 2,
      })
      il.pause()
    end,
  })

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

  -- Debugging startup
  use("dstein64/vim-startuptime")

  -- Sync after packer update/install
  -- Keep last
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
