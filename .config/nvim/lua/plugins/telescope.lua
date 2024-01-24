return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    lazy = false,
    priority = 9998,
    keys = {
      { "<localleader>ff", "<CMD>Telescope find_files<CR>",      desc = "Telescope files" },
      { "<localleader>fg", "<CMD>Telescope git_files<CR>",       desc = "Telescope tracked files" },
      { "<localleader>ft", "<CMD>Telescope live_grep<CR>",       desc = "Telescope live grep" },
      { "<localleader>fb", "<CMD>Telescope buffers<CR>",         desc = "Telescope buffers" },
      { "<localleader>fh", "<CMD>Telescope command_history<CR>", desc = "Telescope command history" },
      { "<localleader>hh", "<CMD>Telescope help_tags<CR>",       desc = "NeoVim help pages" },
      { "<localleader>hm", "<CMD>Telescope man_pages<CR>",       desc = "man pages" },
      { "<localleader>hk", "<CMD>Telescope keymaps<CR>",         desc = "Keymaps" },
      { "<localleader>n",  "<CMD>Telescope notify<CR>",          desc = "Notifications" },
    }
  }
}
