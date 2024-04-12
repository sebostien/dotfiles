local M = {}

---@type _.wezterm.Palette
M.colors = {
  background = "#0D1117",
  foreground = "#FAFAFA",

  cursor_bg = "#999999",
  cursor_fg = "#FFFFFF",
  cursor_border = "#A5A5A5",

  selection_fg = "#FFFFFF",
  selection_bg = "#485460",

  scrollbar_thumb = "#222222",
  split = "#1A1E24",

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
}

---@param config _.wezterm.ConfigBuilder
M.apply_to_config = function(config)
  config.colors = M.colors
  config.colors.tab_bar = { background = M.colors.background }

  config.show_new_tab_button_in_tab_bar = false
  config.use_fancy_tab_bar = false
  config.tab_bar_at_bottom = true
  config.window_padding = {
    left = 2,
    right = 2,
    top = 5,
    bottom = 0,
  }

  config.inactive_pane_hsb = {
    saturation = 1.0,
    brightness = 0.9,
  }

  config.command_palette_bg_color = M.colors.split
  config.command_palette_fg_color = M.colors.foreground

  config.char_select_bg_color = M.colors.split
  config.char_select_fg_color = M.colors.foreground
end

return M
