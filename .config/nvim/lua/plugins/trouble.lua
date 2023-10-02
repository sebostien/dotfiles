return {
  "folke/trouble.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    {
      "<leader>q",
      function() require("trouble").open("document_diagnostics") end,
      desc = "Show document diagnostics"
    },
    {
      "<leader>w",
      function() require("trouble").open("workspace_diagnostics") end,
      desc = "Show workspace diagnostics"
    }
  },
  opts = {
    auto_preview = false,
  },
}
