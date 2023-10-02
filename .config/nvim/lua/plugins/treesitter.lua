return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  opts = {
    -- https://github.com/nvim-treesitter/nvim-treesitter
    ensure_installed = {
      "rust",
      "org",
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
      "markdown_inline",
      "help", -- Vim help
    },
    autopairs = {
      enable = true,
    },
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = { "org" },
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
