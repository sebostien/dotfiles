return {
  {
    "refractalize/oil-git-status.nvim",
    dependencies = {
      "stevearc/oil.nvim",
    },
    config = true,
  },
  {
    'stevearc/oil.nvim',
    opts = {
      win_options = {
        signcolumn = "yes:2",
      },
      skip_confirm_for_simple_edits = true,
      view_options = {
        show_hidden = true,
      },
      float = {
        padding = 8,
        max_width = 128,
      },
      keymaps = {
        ["<ESC>"] = "actions.close",
      }
    },
    lazy = false,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<localleader>fd", function() require("oil").toggle_float() end, desc = "File tree" }
    },
  },
}
