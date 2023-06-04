local wezterm = require("wezterm")
local mux = wezterm.mux

local config = {}
if wezterm.config_builder then
	config = wezterm.config_builder()
end

require("keys")(config)
require("theme")(config)
require("font")(config)
require("status_bar")()
require("launcher")(config)

config.unix_domains = {
	{ name = "unix" },
}

config.adjust_window_size_when_changing_font_size = false

-- wezterm.on("gui-startup", function(cmd)
-- 	local tab, pane, window = mux.spawn_window(cmd or {})
-- end)

return config
