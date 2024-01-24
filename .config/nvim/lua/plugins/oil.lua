return {
  {
    'stevearc/oil.nvim',
    opts = {},
    lazy = false,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<localleader>fd", "<CMD>Oil <CR>", desc = "File tree" }
    },
  },
}
