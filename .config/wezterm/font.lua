local wezterm = require("wezterm")

return function(config)
	config.font = wezterm.font("Hasklug NFM", { weight = "Regular", stretch = "Normal", style = "Normal" })
	config.font_size = 13

	config.font_rules = {
		{
			intensity = "Bold",
			italic = true,
			font = wezterm.font({
				family = "Hasklug NFM",
				weight = "Bold",
				style = "Italic",
			}),
		},
		{
			intensity = "Bold",
			italic = false,
			font = wezterm.font({
				family = "Hasklug NFM",
				weight = "Bold",
				style = "Normal",
			}),
		},
		{
			intensity = "Normal",
			italic = true,
			font = wezterm.font({
				family = "Hasklug NFM",
				weight = "Regular",
				style = "Italic",
			}),
		},
	}
end
