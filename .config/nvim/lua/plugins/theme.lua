-------------------------------------------------
--------------------- Theme ---------------------
-------------------------------------------------

local colors = require("colors")

local WC = colors.window
local SC = colors.syntax

local palettes = {
  nordfox = {
    bg0 = WC.bg0,
    bg1 = WC.bg1,
    black = WC.black,
    white = WC.white,
    red = SC.red,
    green = SC.green,
    yellow = SC.yellow,
    blue = SC.blue,
    magenta = SC.magenta,
    cyan = SC.cyan,
    orange = SC.orange,
  },
}

local specs = {
  nordfox = {
    git = {
      changed = WC.yellow.base,
    },
  },
}

vim.api.nvim_set_hl(0, "NormalFloat", { bg = WC.bg1 })
vim.api.nvim_set_hl(0, "FloatBorder", { bg = WC.bg1 })
vim.api.nvim_set_hl(0, "TelescopeBorder", { bg = WC.bg1 })
vim.api.nvim_set_hl(0, "LspInfoBorder", { bg = WC.bg1, fg = WC.white.base })

return {
  {
    "EdenEast/nightfox.nvim",
    config = function()
      require("nightfox").setup({
        palettes = palettes,
        specs = specs,
        options = {
          terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
          dim_inactive = true,    -- Non focused panes set to alternative background
        },
      })
      vim.cmd([[ colorscheme nordfox ]])
    end
  },
}
