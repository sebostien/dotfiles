return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  opts = {
    -- https://github.com/nvim-treesitter/nvim-treesitter
    ensure_installed = {
      "rust",
      "ron",
      "javascript",
      "typescript",
      "c",
      "cmake",
      "python",
      "toml",
      "yuck",
      "yaml",
      "lua",
      "css",
      "scss",
      "markdown",
      "zathurarc",
      "latex",
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
  },
}
