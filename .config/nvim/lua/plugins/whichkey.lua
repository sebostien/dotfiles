return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = {
        marks = true,
        registers = true,
      },
      window = {
        border = "single",
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)

      wk.register({
        ["<localleader>f"] = { name = "Find stuff", _ = "which_key_ignore" },
        ["<localleader>t"] = { name = "Show TODOs", _ = "which_key_ignore" },
        ["<localleader>h"] = { name = "Help pages", _ = "which_key_ignore" },
        ["<localleader>l"] = { name = "Lazy", _ = "which_key_ignore" },
        ["<localleader>o"] = { name = "Open with program", _ = "which_key_ignore" },
        ["<localleader>g"] = { name = "Git info", _ = "which_key_ignore" },
        ["<localleader>c"] = { name = "Change", _ = "which_key_ignore" },
        ["<localleader>d"] = { name = "DAP", _ = "which_key_ignore" },
        ["<leader>t"] = { name = "Buffer toggles", _ = "which_key_ignore" },
        ["<leader>r"] = { name = "Rename symbols", _ = "which_key_ignore" },
        ["<leader>s"] = { name = "Search with LSP", _ = "which_key_ignore" },
        ["<leader>l"] = { name = "Start LSPs", _ = "which_key_ignore" },
        ["<leader>c"] = { name = "Codeactions LSP", _ = "which_key_ignore" },
      })
    end,
  },
}
