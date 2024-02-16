return {
  {
    "uga-rosa/cmp-dictionary",
    opts = {
      paths = {
        -- spelllang = {
        --   -- To switch:
        --   --   :set spelllang=sv
        --   sv = "/home/sn/.config/nvim/dict/en.dict",
        --   en = "/home/sn/.config/nvim/dict/sv.dict",
        -- },
        first_case_insensitive = true,
        max_number_items = 50,
      },
    },
    config = function(opts)
      require("cmp_dictionary").setup(opts)
    end
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      -- CMP Sources
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-calc",
      -- Formatting
      "onsails/lspkind.nvim",
      -- Snippets
      "quangnguyen30192/cmp-nvim-ultisnips",
      "SirVer/ultisnips",
      -- Dictionary
      "uga-rosa/cmp-dictionary",
    },
    config = function()
      local cmp = require("cmp")
      local types = require("cmp.types")
      local lspkind = require("lspkind")

      WIDE_HEIGHT = 80

      cmp.setup({
        snippet = {
          expand = function(args)
            vim.fn["UltiSnips#Anon"](args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
        sources = {
          {
            name = "nvim_lsp_signature_help",
            priority = 100,
            group_index = 1,
          },
          {
            name = "nvim_lsp",
            priority = 100,
            group_index = 1,
          },
          {
            name = "path",
            priority = 80,
            group_index = 2,
          },
          {
            name = "calc",
            priority = 80,
            group_index = 2,
          },
          {
            name = "buffer",
            priority = 80,
            autocomplete = false,
            group_index = 3,
          },
          {
            name = "crates",
            priority = 70,
            group_index = 3,
          },
          {
            name = "ultisnips",
            priority = 60,
            autocomplete = false,
            group_index = 3,
          },
          {
            name = "dictionary",
            priority = 50,
            keyword_length = 3,
            group_index = 3,
          },
        },
        confirmation = {
          default_behavior = types.cmp.ConfirmBehavior.Replace,
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = {
            border = "rounded",
            maxwidth = math.floor(WIDE_HEIGHT * (vim.o.columns / 100)),
            maxheight = math.floor(WIDE_HEIGHT * (vim.o.lines / 100)),
          },
        },
        formatting = {
          format = lspkind.cmp_format({
            with_text = true,
            maxwidth = 50,
          }),
        },
      })
    end,
  },
}
