local wezterm = require("wezterm")
local act = wezterm.action

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

local show_unicode_select = wezterm.action_callback(function(window, pane)
  window:perform_action(
    act.InputSelector({
      title = "Select type",
      choices = {
        { id = "RecentlyUsed", label = "Recently used" },
        { id = "UnicodeNames", label = "All unicode" },
        { id = "NerdFonts", label = "Nerd Fonts" },
        { id = "Flags", label = "Flags" },
        { id = "SmileysAndEmotion", label = "Smileys" },
        { id = "PeopleAndBody", label = "People" },
        { id = "AnimalsAndNature", label = "Animals and Nature" },
        { id = "FoodAndDrink", label = "Food and Drinks" },
        { id = "TravelAndPlaces", label = "Travel and Places" },
        { id = "Activities", label = "Activites" },
        { id = "Objects", label = "Objects" },
        { id = "Symbols", label = "Symbols" },
      },
      action = wezterm.action_callback(function(_, _, id, _)
        if id and #id > 0 then
          window:perform_action(
            act.CharSelect({
              copy_on_select = true,
              copy_to = "ClipboardAndPrimarySelection",
              group = id,
            }),
            pane
          )
        end
      end),
    }),
    pane
  )
end)

config.keys = {
  { key = "r", mods = "CTRL|SHIFT", action = act.ReloadConfiguration },
  { key = "p", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
  { key = "o", mods = "CTRL|SHIFT", action = act.ShowDebugOverlay },
  { key = "u", mods = "CTRL|SHIFT", action = show_unicode_select },

  -- Clipboard
  { key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },

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
