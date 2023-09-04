local wezterm = require("wezterm")

local config = {}
if wezterm.config_builder then
  config = wezterm.config_builder()
end

require("keys")(config)
require("theme")(config)
require("font")(config)
require("launcher")(config)
-- require("status_bar")()

config.adjust_window_size_when_changing_font_size = false
config.enable_tab_bar = false

return config
