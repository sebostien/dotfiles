-------------------------------------------------
--------------------- Theme ---------------------
-------------------------------------------------

local colors = {}

colors.window = {
	bg1 = "#13171d",
	bg0 = "#0d1117",

	black = {
		base = "#0d1117",
		bright = "#4a576f",
	},
	white = {
		base = "#fafafa",
		bright = "#ffffff",
	},
	red = {
		base = "#F44336",
		bright = "#ff5252",
	},
	green = {
		base = "#27ae60",
		bright = "#2ecc71",
	},
	yellow = {
		base = "#Fa951a",
		bright = "#FBC02D",
	},
	blue = {
		base = "#2880fe",
		bright = "#03A9F4",
	},
	magenta = {
		base = "#9C27B0",
		bright = "#EA80FC",
	},
	cyan = {
		base = "#00c8d4",
		bright = "#34e7e4",
	},
}

colors.syntax = {
	red = {
		base = "#ff6c6b",
		bright = "#e45649",
	},
	green = {
		base = "#98be65",
		bright = "#50a14f",
	},
	orange = {
		base = "#da8548",
		bright = "#da8548",
	},
	yellow = {
		base = "#ecbe7b",
		bright = "#986801",
	},
	blue = {
		base = "#51afef",
		bright = "#4078f2",
	},
	magenta = {
		base = "#c678dd",
		bright = "#a626a4",
	},
	cyan = {
		base = "#46d9ff",
		bright = "#0184bc",
	},
}

colors.Syntax = {}

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

require("nightfox").setup({
	palettes = palettes,
	specs = specs,
	options = {
		transparent = false, -- Disable setting background
		terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
		dim_inactive = true, -- Non focused panes set to alternative background
	},
})

vim.cmd("colorscheme nordfox")

return colors
