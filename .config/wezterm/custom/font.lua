local wezterm = require("wezterm")

local M = {}

---@param config _.wezterm.ConfigBuilder
M.apply_to_config = function(config)
  config.font = wezterm.font("Fira Code", {
    weight = "Regular",
    stretch = "Normal",
    style = "Normal",
  })
  config.font_size = 14

  config.font_rules = {
    {
      intensity = "Bold",
      italic = true,
      font = wezterm.font({
        family = "Fira Code",
        weight = "Bold",
        style = "Italic",
      }),
    },
    {
      intensity = "Bold",
      italic = false,
      font = wezterm.font({
        family = "Fira Code",
        weight = "Bold",
        style = "Normal",
      }),
    },
    {
      intensity = "Normal",
      italic = true,
      font = wezterm.font({
        family = "Fira Code",
        weight = "Regular",
        style = "Italic",
      }),
    },
  }
end

return M
