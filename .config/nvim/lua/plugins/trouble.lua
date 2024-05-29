return {
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      {
        "<leader>q",
        function()
          require("trouble").open("document_diagnostics")
        end,
        desc = "Trouble document diagnostics",
      },
      {
        "<leader>w",
        function()
          require("trouble").open("workspace_diagnostics")
        end,
        desc = "Trouble workspace diagnostics",
      },
      {
        "<localleader>q",
        function()
          require("trouble").open("quickfix")
        end,
        desc = "Trouble quickfix",
      },
    },
    opts = {
      auto_preview = false,
    },
  },
}
