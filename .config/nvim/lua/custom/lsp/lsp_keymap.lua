local M = {}

M.bufopts = function(bufnr, desc)
  return {
    noremap = true,
    silent = true,
    buffer = bufnr,
    desc = desc,
  }
end

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { noremap = true, silent = true, desc = "Go to prev diagnostics" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { noremap = true, silent = true, desc = "Go to mext diagnostics" })
vim.keymap.set(
  "n",
  "<space>q",
  "<CMD>Trouble document_diagnostics<CR>",
  { noremap = true, silent = true, desc = "Show all diagnostics" }
)
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, M.bufopts(bufnr, "Format file"))

-- https://github.com/neovim/nvim-lspconfig
M.on_attach = function(_, bufnr)
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, M.bufopts(bufnr, "Go to definition"))
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, M.bufopts(bufnr, "Go to declaration"))
  vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, M.bufopts(bufnr, "Go to type definition"))
  vim.keymap.set("n", "gr", vim.lsp.buf.references, M.bufopts(bufnr, "Go to references"))
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, M.bufopts(bufnr, "Go to implementations"))

  vim.keymap.set("n", "K", vim.lsp.buf.hover, M.bufopts(bufnr, "Hover"))
  vim.keymap.set("n", "<leader>k", vim.lsp.buf.signature_help, M.bufopts(bufnr, "Signature help"))
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, M.bufopts(bufnr, "Rename"))
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, M.bufopts(bufnr, "Code actions"))
end

return M
