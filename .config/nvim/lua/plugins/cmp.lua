return {
  "hrsh7th/nvim-cmp",
  event = "VeryLazy",
  dependencies = {
    -- CMP Sources
    { "hrsh7th/cmp-nvim-lsp-signature-help" },
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/cmp-path" },
    { "hrsh7th/cmp-calc" },
    -- Formatting
    { "onsails/lspkind.nvim" },
    -- Snippets
    {
      "L3MON4D3/LuaSnip",
      build = "make install_jsregexp",
      config = function()
        require("luasnip.loaders.from_snipmate").lazy_load()
      end,
    },
    { "saadparwaiz1/cmp_luasnip" },
    -- Dictionary
    {
      "uga-rosa/cmp-dictionary",
      opts = {
        paths = {
          -- spelllang = {
          --   --   -- To switch:
          --   --   --   :set spelllang=sv
          --   sv = "/home/sn/.config/nvim/dict/en.dict",
          --   en = "/home/sn/.config/nvim/dict/sv.dict",
          -- },
          first_case_insensitive = true,
          max_number_items = 50,
        },
      },
      config = function(_, opts)
        -- TODO: Dict completion
        -- require("cmp_dictionary").setup(opts)
      end,
    },
  },
  config = function()
    local cmp = require("cmp")
    local types = require("cmp.types")
    local lspkind = require("lspkind")
    local luasnip = require("luasnip")

    luasnip.config.setup({})
    WIDE_HEIGHT = 80

    cmp.setup({
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      completion = { completeopt = "menu,menuone,noinsert" },
      mapping = cmp.mapping.preset.insert({
        -- Select
        ["<C-n>"] = cmp.mapping.select_next_item,
        ["<C-p>"] = cmp.mapping.select_prev_item,
        -- Scroll docs
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        -- Move snippet expansion
        ["<C-l>"] = cmp.mapping(function()
          if luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          end
        end, { "i", "s" }),
        ["<C-h>"] = cmp.mapping(function()
          if luasnip.locally_jumpable(-1) then
            luasnip.jump(-1)
          end
        end, { "i", "s" }),

        -- Other
        ["<C-e>"] = cmp.mapping.abort(),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
      }),
      -----
      sources = {
        { name = "nvim_lsp_signature_help" },
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "path" },
        { name = "calc" },
        { name = "buffer" },
        { name = "dictionary", keyword_length = 3 },
      },
      confirmation = {
        -- TODO: This
        default_behavior = types.cmp.ConfirmBehavior.Replace,
      },
      window = {
        -- TODO: This
        documentation = {
          maxwidth = math.floor(WIDE_HEIGHT * (vim.o.columns / 100)),
          maxheight = math.floor(WIDE_HEIGHT * (vim.o.lines / 100)),
        },
      },
      formatting = {
        -- TODO: This
        format = lspkind.cmp_format({
          with_text = true,
          maxwidth = 50,
        }),
      },
    })
  end,
}
