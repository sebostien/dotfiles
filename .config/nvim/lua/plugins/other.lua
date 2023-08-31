return {
  ---- Other ----
  "stevearc/dressing.nvim", -- Make stuff prettier

  -- Indentation Guides
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({})
    end,
  },


  -- Undotree
  "mbbill/undotree",

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
    config = function()
      require("nvim-autopairs").setup({})
    end,
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
      il.pause()
    end,
  },
}
