-- Keys for wezterm.
-- Almost everything is disabled in favor of TMUX

local wezterm = require("wezterm")
local act = wezterm.action

return function(config)
  config.disable_default_key_bindings = true
  config.warn_about_missing_glyphs = false

  config.keys = {
    { key = "o", mods = "CTRL|SHIFT", action = act.ShowDebugOverlay },
    { key = "p", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
    { key = "r", mods = "CTRL|SHIFT", action = act.ReloadConfiguration },
    {
      key = "u",
      mods = "CTRL|SHIFT",
      action = act.CharSelect({
        copy_on_select = true,
        copy_to = "ClipboardAndPrimarySelection",
        group = "NerdFonts",
      }),
    },

    -- Font size
    { key = "0", mods = "CTRL",       action = act.ResetFontSize },
    { key = "=", mods = "CTRL",       action = act.IncreaseFontSize },
    { key = "-", mods = "CTRL",       action = act.DecreaseFontSize },

    -- Clipboard
    { key = "C", mods = "CTRL|SHIFT", action = act.CopyTo("Clipboard") },
    { key = "V", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },
  }
end
