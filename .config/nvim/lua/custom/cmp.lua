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
      name = "spell",
      priority = 50,
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
