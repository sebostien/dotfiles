---- Other ----
return {
  -- Personal
  {
    dir = "~/Documents/GitHub/unicode.nvim/",
    opts = { },
  },
  -- Make stuff prettier
  "stevearc/dressing.nvim",
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
        animation = "static",
      })
    end
  },

  -- indent guides for Neovim
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      indent = { char = "│" },
      scope = { enabled = false },
      exclude = {
        filetypes = {
          "help",
          "neo-tree",
          "Trouble",
          "lazy",
          "mason",
          "notify",
        },
      },
    },
    main = "ibl",
  },

  {
    "echasnovski/mini.indentscope",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local mini = require("mini.indentscope")

      mini.setup({
        symbol = "│",
        options = { try_as_border = true },
        draw = {
          animation = mini.gen_animation.quadratic({ easing = 'out', duration = 10 })
        }
      })

      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "help",
          "neo-tree",
          "Trouble",
          "lazy",
          "mason",
          "notify",
        },
        callback = function()
          ---@diagnostic disable-next-line
          vim.b.miniindentscope_disable = true
        end,
      })
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
    keys = {
      { "<leader>tc", "<CMD>CccHighlighterToggle<CR>", desc = "Toggle color preview" }

    }
  },

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
      { "<leader>ti", "<CMD>IlluminateToggle<CR>", desc = "Toggle word highlight" },
    },
  },
}
