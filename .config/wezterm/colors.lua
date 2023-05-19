return function(config)
	config.color_scheme = "nightfox"
	config.colors = {
		background = "#0D1117",
		foreground = "#FAFAFA",

		cursor_bg = "#999999",
		cursor_fg = "#FFFFFF",
		cursor_border = "#A5A5A5",

		selection_fg = "#FFFFFF",
		selection_bg = "#4a576f",

		-- The color of the scrollbar "thumb"; the portion that represents the current viewport
		scrollbar_thumb = "#222222",

		-- The color of the split lines between panes
		split = "#444444",

		ansi = {
			"#0D1117",
			"#F44336",
			"#27AE60",
			"#FA951A",
			"#2880FE",
			"#9C27B0",
			"#00C8D4",
			"#FAFAFA",
		},
		brights = {
			"#485460",
			"#FF5252",
			"#2ECC71",
			"#FBC02D",
			"#03A9F4",
			"#EA80FC",
			"#34E7E4",
			"#FFFFFF",
		},

		-- Arbitrary colors of the palette in the range from 16 to 255
		-- indexed = { [136] = "#af8700" },

		-- Since: 20220319-142410-0fcdea07
		-- When the IME, a dead key or a leader key are being processed and are effectively
		-- holding input pending the result of input composition, change the cursor
		-- to this color to give a visual cue about the compose state.
		compose_cursor = "#FA951A",
	}
end
