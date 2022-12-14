require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = { "sumneko_lua", "tsserver", "jsonls", "rust_analyzer" },
})

require("lspconfig.ui.windows").default_options.border = "rounded"

local function bufopts(bufnr, desc)
  return {
    noremap = true,
    silent = true,
    buffer = bufnr,
    desc = desc,
  }
end

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set(
  "n",
  "[d",
  vim.diagnostic.goto_prev,
  { noremap = true, silent = true, desc = "Go to prev diagnostics" }
)
vim.keymap.set(
  "n",
  "]d",
  vim.diagnostic.goto_next,
  { noremap = true, silent = true, desc = "Go to mext diagnostics" }
)
vim.keymap.set(
  "n",
  "<space>q",
  vim.diagnostic.setloclist,
  { noremap = true, silent = true, desc = "Show all diagnostics" }
)

-- https://github.com/neovim/nvim-lspconfig
local on_attach = function(_, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set("n", "<leader>gD", vim.lsp.buf.declaration, bufopts(bufnr, "Go to declaration"))
  vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, bufopts(bufnr, "Go to definition"))
  vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, bufopts(bufnr, "Go to references"))
  vim.keymap.set(
    "n",
    "<leader>gi",
    vim.lsp.buf.implementation,
    bufopts(bufnr, "Go to implementations")
  )

  vim.keymap.set("n", "<leader>K", vim.lsp.buf.hover, bufopts(bufnr, "Hover"))
  vim.keymap.set("n", "<leader>k", vim.lsp.buf.signature_help, bufopts(bufnr, "Signature help"))
  vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, bufopts(bufnr, "Type definition"))
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts(bufnr, "Rename"))
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, bufopts(bufnr, "Code actions"))
  vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, bufopts(bufnr, "Format file"))
end

require("lspconfig")["eslint"].setup({
  on_attach = on_attach,
  -- Server-specific settings...
  capabilities = vim.lsp.protocol.make_client_capabilities(),
  settings = {
    packageManager = "yarn",
  },
})

require("lspconfig")["tsserver"].setup({
  on_attach = function(client, bufnr)
    client.server_capabilities.documentFormattingProvider = false
    on_attach(client, bufnr)
  end,
  -- Server-specific settings...
  capabilities = vim.lsp.protocol.make_client_capabilities(),
  settings = {},
})

require("lspconfig")["rust_analyzer"].setup({
  on_attach = on_attach,
  -- Server-specific settings...
  capabilities = vim.lsp.protocol.make_client_capabilities(),
  settings = {},
})

require("neodev").setup()
require("lspconfig")["sumneko_lua"].setup({
  on_attach = function(client, bufnr)
    client.server_capabilities.documentFormattingProvider = false
    on_attach(client, bufnr)
  end,
  capabilities = vim.lsp.protocol.make_client_capabilities(),
  -- Server-specific settings...
  settings = {
    Lua = {
      completion = {
        callSnipper = "Replace",
      },
      diagnostics = {
        globals = { "vim" },
      },
    },
  },
})

require("lspconfig")["jsonls"].setup({
  on_attach = on_attach,
  capabilities = vim.lsp.protocol.make_client_capabilities(),
  -- Server-specific settings...
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
})

require("lspconfig")["marksman"].setup({
  on_attach = on_attach,
  capabilities = vim.lsp.protocol.make_client_capabilities(),
})

require("lspconfig")["hls"].setup({
  on_attach = on_attach,
  capabilities = vim.lsp.protocol.make_client_capabilities(),
})

require("lspconfig")["cssls"].setup({
  on_attach = on_attach,
  capabilities = vim.lsp.protocol.make_client_capabilities(),
  settings = {}
})
