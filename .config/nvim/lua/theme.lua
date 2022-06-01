-------------------------------------------------
--------------------- Theme ---------------------
-------------------------------------------------
local colors = require('colors')
local WindowColors = colors.window

local palettes = {
    nightfox = {
        bg1 = WindowColors.bg1,
        bg0 = WindowColors.bg1,
        red = WindowColors.red
    }
}

local specs = {
    nightfox = {
        git = {
            changed = "orange"
        }
    }
}

require('nightfox').setup({
    palettes = palettes,
    specs = specs,
    options = {
        transparent = false, -- Disable setting background

        terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`

        dim_inactive = true -- Non focused panes set to alternative background
    }
})

vim.cmd("colorscheme nightfox")
