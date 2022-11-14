vim.o.termguicolors = true

local vi_mode_utils = require("feline.providers.vi_mode")
local theme_colors = require("theme").window

local force_inactive = {
	filetypes = {
		"^NvimTree$",
		"^packer$",
		"^startify$",
		"^fugitive$",
		"^fugitiveblame$",
		"^help$",
	},
	buftypes = {
		"^terminal$",
	},
	bufnames = {},
}

local disable = {
	filetypes = {
		"^aerial$",
		"^qf$",
		"^neo.tree$",
	},
}

local colors = {
	bg = theme_colors.bg0,
	fg = theme_colors.white.base,
	black = theme_colors.bg1,
	gray = "#161d27",
	white = theme_colors.white.base,
	yellow = theme_colors.yellow.base,
	cyan = theme_colors.cyan.base,
	blue = theme_colors.blue.base,
	oceanblue = theme_colors.cyan.bright,
	green = theme_colors.green.base,
	orange = theme_colors.yellow.base,
	violet = theme_colors.magenta.bright,
	magenta = theme_colors.magenta.base,
	skyblue = theme_colors.blue.base,
	red = theme_colors.red.base,
}

local M = {
	statusline = {
		active = { {}, {}, {} },
		inactive = { {}, {} },
	},
	winbar = {
		active = {},
		inactive = {},
	},
}

M.statusline.active[1] = {
	{
		provider = "",
		hl = function()
			return {
				name = vi_mode_utils.get_mode_highlight_name(),
				fg = vi_mode_utils.get_mode_color(),
				style = "bold",
			}
		end,
		left_sep = {
			"slant_left_2",
		},
	},
	{
		provider = "file_type",
		hl = {
			fg = "white",
			bg = "blue",
		},
		left_sep = {
			"slant_left_2",
			{
				str = " ",
				hl = {
					bg = "blue",
					fg = "NONE",
				},
			},
		},
		right_sep = {
			{ str = " ", hl = { bg = "blue", fg = "NONE" } },
			"slant_right",
		},
	},
	{
		provider = "file_info",
		opts = {
			type = "relative",
		},
		hl = {
			fg = "white",
			bg = "blue",
			style = "bold",
		},
		left_sep = {
			"slant_left_2",
			{ str = " ", hl = { bg = "blue", fg = "NONE" } },
		},
		right_sep = {
			{ str = " ", hl = { bg = "blue", fg = "NONE" } },
			"slant_right",
			" ",
		},
	},
	{
		provider = "position",
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_left_2_thin",
				hl = {
					fg = "fg",
					bg = "bg",
				},
			},
		},
	},
	{
		provider = "search_count",
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_left_2_thin",
				hl = {
					fg = "fg",
					bg = "bg",
				},
			},
		},
	},
	{
		provider = "diagnostic_errors",
		hl = { fg = "red" },
	},
	{
		provider = "diagnostic_warnings",
		hl = { fg = "yellow" },
	},
	{
		provider = "diagnostic_hints",
		hl = { fg = "cyan" },
	},
	{
		provider = "diagnostic_info",
		hl = { fg = "skyblue" },
	},
}

M.statusline.active[2] = {
	{
		provider = "git_branch",
		hl = {
			fg = "yellow",
			bg = "bg",
			style = "bold",
		},
	},
	{
		provider = "git_diff_added",
		hl = {
			fg = "green",
		},
	},
	{
		provider = "git_diff_changed",
		hl = {
			fg = "orange",
		},
	},
	{
		provider = "git_diff_removed",
		hl = {
			fg = "red",
		},
	},
}

M.statusline.active[3] = {
	{
		provider = "lsp_client_names",
		hl = { fg = "yellow", style = "bold" },
		right_sep = {
			" ",
			{
				str = "slant_right_2_thin",
				hl = { fg = "fg", bg = "bg" },
			},
		},
	},
	{
		provider = "file_encoding",
		hl = {
			style = "bold",
		},
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_right_2_thin",
				hl = { fg = "fg", bg = "bg" },
			},
		},
	},
	{
		provider = "file_size",
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_right_2_thin",
				hl = { fg = "fg", bg = "bg" },
			},
			" ",
		},
	},
	{
		provider = "line_percentage",
		hl = {
			style = "bold",
		},
		right_sep = " ",
	},
}

M.statusline.inactive[1] = {
	{
		provider = "",
		hl = function()
			return {
				fg = "white",
				style = "bold",
			}
		end,
		left_sep = {
			"slant_left_2",
		},
	},
	{
		provider = "file_type",
		hl = {
			fg = "white",
			bg = "blue",
		},
		left_sep = {
			"slant_left_2",
			{
				str = " ",
				hl = {
					bg = "blue",
					fg = "NONE",
				},
			},
		},
		right_sep = {
			{ str = " ", hl = { bg = "blue", fg = "NONE" } },
			"slant_right",
		},
	},
	{
		provider = "file_info",
		hl = {
			fg = "white",
			bg = "blue",
			style = "bold",
		},
		left_sep = {
			"slant_left_2",
			{ str = " ", hl = { bg = "blue", fg = "NONE" } },
		},
		right_sep = {
			{ str = " ", hl = { bg = "blue", fg = "NONE" } },
			"slant_right",
			" ",
		},
	},
	{
		provider = "position",
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_left_2_thin",
				hl = {
					fg = "fg",
					bg = "bg",
				},
			},
		},
	},
}

M.statusline.inactive[2] = {
	{
		provider = "file_encoding",
		hl = {
			style = "bold",
		},
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_right_2_thin",
				hl = { fg = "fg", bg = "bg" },
			},
		},
	},
	{
		provider = "file_size",
		left_sep = " ",
		right_sep = {
			" ",
			{
				str = "slant_right_2_thin",
				hl = { fg = "fg", bg = "bg" },
			},
			" ",
		},
	},
	{
		provider = "line_percentage",
		hl = {
			style = "bold",
		},
		right_sep = " ",
	},
}

M.winbar.active[1] = {
	{
		provider = "file_info",
		opts = {
			type = "unique",
		},
		hl = {
			fg = "green",
			bg = "gray",
			style = "bold",
		},
		left_sep = {
			" ",
			{
				str = "slant_left_2",
				hl = {
					fg = "gray",
					bg = "bg",
				},
			},
			{ str = " ", hl = { bg = "gray" } },
		},
		right_sep = {
			{ str = " ", hl = { bg = "gray" } },
			"slant_right_2",
		},
	},
	{
		provider = " ",
		hl = {
			fg = "NONE",
			bg = "bg",
		},
	},
}

M.winbar.inactive[1] = {
	{
		provider = "file_info",
		opts = {
			type = "unique",
		},
		hl = {
			fg = "white",
			bg = "gray",
			style = "bold",
		},
		left_sep = {
			" ",
			{
				str = "slant_left_2",
				hl = {
					fg = "gray",
					bg = "bg",
				},
			},
			{ str = " ", hl = { bg = "gray" } },
		},
		right_sep = {
			{ str = " ", hl = { bg = "gray" } },
			"slant_right_2",
		},
	},
	{
		provider = " ",
		hl = {
			fg = "NONE",
			bg = "bg",
		},
	},
}

require("feline").setup({
	theme = colors,
	components = M.statusline,
	default_bg = theme_colors.bg0,
	default_fg = theme_colors.white.base,
	disable = disable,
	force_inactive = force_inactive,
})

require("feline").winbar.setup({
	theme = colors,
	components = M.winbar,
	default_bg = theme_colors.bg0,
	default_fg = theme_colors.white.base,
	disable = disable,
	force_inactive = force_inactive,
})
