return {

  ---- Other ----
  "stevearc/dressing.nvim", -- Make stuff prettier
  {
    "rcarriga/nvim-notify",
    priority = 9997,
    config = function()
      local notify = require("notify")
      vim.notify = notify
      ---@diagnostic disable-next-line
      notify.setup({
        background_colour = "#0d1117",
        render = "compact",
        timeout = 10000,
      })
    end
  },

  -- Indentation Guides
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({})
    end,
  },


  -- Undotree
  {
    "mbbill/undotree",
    keys = {
      { "<localleader>u", "<CMD>UndotreeToggle<CR>", desc = "UndoTree toggle" },
    }
  },

  -- Color picker/preview
  {
    "uga-rosa/ccc.nvim",
    config = function()
      -- Enable true color
      vim.opt.termguicolors = true
    end,
  },

  -- Diagnostics on sepereate lines
  -- use({
  --   "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  --   config = function()
  --     require("lsp_lines").setup()
  --     require("lsp_lines").toggle()
  --     -- vim.diagnostics.config({ virtual_lines = false })
  --   end,
  -- })

  -- Autopairs
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {
      check_ts = true,
    }
  },

  -- Shortcuts for surrounding
  {
    'echasnovski/mini.surround',
    version = '*',
    opts = {}
  },

  -- Highlight word under cursor
  {
    "RRethy/vim-illuminate",
    config = function()
      local il = require("illuminate")
      il.configure({
        under_cursor = false,
        min_count_to_highlight = 2,
      })
    end,
    keys = {
      "<leader>ti", "<CMD>IlluminateToggle<CR>", desc = "Toggle word highlight"
    },
  },
}
