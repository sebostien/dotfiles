local wezterm = require("wezterm")
local act = wezterm.action
local H = require("helper")

local config = wezterm.config_builder()

config.disable_default_key_bindings = true
config.warn_about_missing_glyphs = false
config.adjust_window_size_when_changing_font_size = false
config.enable_tab_bar = false
config.detect_password_input = true
config.check_for_updates = false
config.scrollback_lines = 25000
config.tab_max_width = 32

require("custom.font").apply_to_config(config)
require("custom.theme").apply_to_config(config)

config.keys = {
  { key = "r", mods = "CTRL|SHIFT", action = act.ReloadConfiguration },
  { key = "p", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
  { key = "o", mods = "CTRL|SHIFT", action = act.ShowDebugOverlay },
  { key = "u", mods = "CTRL|SHIFT", action = H.show_unicode_select },
  {
    key = "f",
    mods = "CTRL|SHIFT",
    action = act.QuickSelectArgs({
      action = wezterm.action_callback(function(window, pane)
        local url = window:get_selection_text_for_pane(pane)
        wezterm.open_with(url, "xdg-open")
      end),
    }),
  },

  -- Clipboard
  { key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom "Clipboard" },

  -- Font size
  {
    key = "0",
    mods = "CTRL",
    action = act.ResetFontSize,
  },
  { key = "=", mods = "CTRL", action = act.IncreaseFontSize },
  { key = "-", mods = "CTRL", action = act.DecreaseFontSize },
}

return config
