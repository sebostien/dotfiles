local M = {}

M.bufopts = function(bufnr, desc)
  return {
    noremap = true,
    silent = true,
    buffer = bufnr,
    desc = desc,
  }
end

-- https://github.com/neovim/nvim-lspconfig
M.on_attach = function(_, bufnr)
  -- Diagnostics
  -- `:help vim.diagnostic.*`
  vim.keymap.set(
    "n",
    "<space>e",
    vim.diagnostic.open_float,
    { noremap = true, silent = true, desc = "Open diagnostics float" }
  )
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { noremap = true, silent = true, desc = "Go to prev diagnostics" })
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { noremap = true, silent = true, desc = "Go to mext diagnostics" })

  -- Gotos
  -- See `:help vim.lsp.*`
  vim.keymap.set("n", "gr", vim.lsp.buf.references, M.bufopts(bufnr, "Go to references"))
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, M.bufopts(bufnr, "Go to definition"))
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, M.bufopts(bufnr, "Go to declaration"))
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, M.bufopts(bufnr, "Go to implementations"))
  vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, M.bufopts(bufnr, "Go to type definition"))

  -- Other
  vim.keymap.set("n", "K", vim.lsp.buf.hover, M.bufopts(bufnr, "Hover"))
  vim.keymap.set("n", "<leader>k", vim.lsp.buf.signature_help, M.bufopts(bufnr, "Signature help"))
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, M.bufopts(bufnr, "Rename"))
  -- TODO: Simple rust-tools like popup
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, M.bufopts(bufnr, "Code actions"))

  -- Format
  vim.keymap.set("n", "<leader>f", function()
      vim.lsp.buf.format({ async = true })
    end,
    M.bufopts(bufnr, "Format file")
  )
end

return M
