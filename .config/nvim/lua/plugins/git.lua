return {
  -- Git decorations
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        untracked = { text = " " },
      },
    },
  },
  {
    "tpope/vim-fugitive",
    keys = {
      "<localleader>gs", "<CMD>Git<CR>", desc = "Fugitive git status",
    }
  }
}
