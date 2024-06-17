--- Simple parser for justfile errors

vim.api.nvim_create_autocmd("FileType", {
  pattern = "just",
  callback = function(_)
    vim.lsp.start({
      name = "any_ls",
      cmd = { "any_ls" },
      root_dir = "/home/sn/",
    })
  end,
})

-- local namespace = vim.api.nvim_create_namespace("just")
--
-- vim.api.nvim_create_autocmd({ "BufWritePost" }, {
--   pattern = "justfile",
--   callback = function(_)
--     local message = vim.fn.system("just -n")
--     local diagnostics = {}
--     local bufnr = vim.fn.bufnr()
--
--     if vim.v.shell_error ~= 0 then
--       local s_row, s_col = message:match("——▶.*:(%d+):(%d+)")
--       local row = tonumber(s_row)
--       local col = tonumber(s_col)
--
--       table.insert(diagnostics, {
--         severity = vim.diagnostic.severity.ERROR,
--         lnum = (row or 1) - 1,
--         col = (col or 1) - 1,
--         message = message
--       })
--
--       vim.diagnostic.set(namespace, bufnr, diagnostics)
--     else
--       vim.diagnostic.set(namespace, bufnr, diagnostics)
--     end
--   end,
-- })
