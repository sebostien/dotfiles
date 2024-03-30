--- conform.nvim - Lightweight yet powerful formatter plugin for Neovim
--- https://github.com/stevearc/conform.nvim
return {
  "stevearc/conform.nvim",
  keys = {
    {
      "<leader>cf",
      function()
        require("conform").format()
      end,
      desc = "Format with conform.nvim",
    },
  },
  cmd = "ConformInfo",
  --- @class ConformOpts
  opts = {
    format = {
      timeout_ms = 3000,
      async = false,
      quiet = false,
    },
    ---@type = table<string, conform.FormatterUnit[]>
    formatters_by_ft = {
      -- Conform will run multiple formatters sequentially
      -- Use a sub-list to run only the first available formatter
      lua = { "stylua" },
      python = { "isort", "black" },
      javascript = { { "prettierd", "prettier" } },
      typescript = { { "prettierd", "prettier" } },
      sh = { "shfmt" },
      rust = { "rustfmt" },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_fallback = true,
    },
  },
}
