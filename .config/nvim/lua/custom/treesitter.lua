-- https://github.com/nvim-treesitter/nvim-treesitter
require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "rust",
    "javascript",
    "typescript",
    "c",
    "cmake",
    "python",
    "lua",
    "css",
    "scss",
    "markdown",
    "markdown_inline",
    "help", -- Vim help
  },

  autopairs = {
    enable = true,
  },
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
    disable = { "yaml" },
  },
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
})
