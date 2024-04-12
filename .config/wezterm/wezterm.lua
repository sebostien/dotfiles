local wezterm = require("wezterm")
local act = wezterm.action
local H = require("helper")
local workspaces = require("custom.workspaces")

local config = wezterm.config_builder()

config.disable_default_key_bindings = true
config.warn_about_missing_glyphs = false
config.adjust_window_size_when_changing_font_size = false
config.enable_tab_bar = true
config.detect_password_input = true
config.check_for_updates = false
config.scrollback_lines = 25000
config.tab_max_width = 32

config.unix_domains = {
  { name = 'unix', },
}

config.launch_menu = {
  { label = "Calculator", args = { "kalker" } },
  { label = "Calendar",   args = { "bash", "-c", "cal -ymw && read" } },
}

require("custom.theme").apply_to_config(config)
require("custom.font").apply_to_config(config)
require("custom.status_bar").apply_to_config(config)

require("setup").apply_to_config(config, {
  leader = { key = "a", mods = "CTRL", timeout_milliseconds = 60 * 1000 },
  keys = {
    normal = {
      -- Wezterm
      { key = "r",  mods = "CTRL|SHIFT", action = act.ReloadConfiguration },
      { key = "\\", mods = "LEADER",     action = act.ActivateCommandPalette },
      { key = "o",  mods = "CTRL|SHIFT", action = act.ShowDebugOverlay, },
      { key = "u",  mods = "LEADER",     action = H.show_unicode_select,     desc = "Find unicode char" },
      { key = "f",  mods = "LEADER",     action = act.QuickSelect,           desc = "Enter QuickSelect" },
      {
        key = "F",
        mods = "LEADER",
        action = act.QuickSelectArgs({
          action = wezterm.action_callback(function(window, pane)
            local url = window:get_selection_text_for_pane(pane)
            wezterm.open_with(url, "xdg-open")
          end)
        }),
        desc = "QuickSelect and open with xdg-open"
      },

      -- Font size
      { key = "0", mods = "CTRL",         action = act.ResetFontSize,                                    desc = "Reset font size" },
      { key = "=", mods = "CTRL",         action = act.IncreaseFontSize },
      { key = "-", mods = "CTRL",         action = act.DecreaseFontSize },

      -- Workspaces
      { key = "$", mods = "LEADER|SHIFT", action = H.rename_workspace,                                   desc = "Rename workspace" },
      { key = "w", mods = "LEADER",       action = workspaces.select,                                    desc = "Select workspace" },

      -- Tabs
      { key = "R", mods = "LEADER|SHIFT", action = H.rename_tab,                                         desc = "Rename tab" },
      { key = "c", mods = "LEADER",       action = act.SpawnTab("CurrentPaneDomain") },
      { key = "&", mods = "LEADER|SHIFT", action = act.CloseCurrentTab({ confirm = true }),              desc = "Close current tab" },
      { key = "p", mods = "LEADER",       action = act.ActivateTabRelative(-1) },
      { key = "n", mods = "LEADER",       action = act.ActivateTabRelative(1) },
      { key = "1", mods = "LEADER",       action = act.ActivateTab(0) },
      { key = "2", mods = "LEADER",       action = act.ActivateTab(1) },
      { key = "3", mods = "LEADER",       action = act.ActivateTab(2) },
      { key = "4", mods = "LEADER",       action = act.ActivateTab(3) },
      { key = "5", mods = "LEADER",       action = act.ActivateTab(4) },
      { key = "6", mods = "LEADER",       action = act.ActivateTab(5) },
      { key = "7", mods = "LEADER",       action = act.ActivateTab(6) },
      { key = "8", mods = "LEADER",       action = act.ActivateTab(7) },
      { key = "9", mods = "LEADER",       action = act.ActivateTab(8) },

      -- Panes
      { key = "v", mods = "LEADER",       action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
      { key = "s", mods = "LEADER",       action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
      { key = "{", mods = "LEADER",       action = act.RotatePanes("CounterClockwise") },
      { key = "}", mods = "LEADER",       action = act.RotatePanes("Clockwise") },
      { key = "h", mods = "LEADER",       action = act.ActivatePaneDirection("Left") },
      { key = "j", mods = "LEADER",       action = act.ActivatePaneDirection("Down") },
      { key = "k", mods = "LEADER",       action = act.ActivatePaneDirection("Up") },
      { key = "l", mods = "LEADER",       action = act.ActivatePaneDirection("Right") },
      { key = "q", mods = "LEADER",       action = act.PaneSelect({ mode = "Activate" }) },
      { key = "z", mods = "LEADER",       action = act.TogglePaneZoomState },
      { key = "!", mods = "LEADER",       action = H.move_pane_to_new_tab },
      { key = "H", mods = "LEADER|SHIFT", action = act.AdjustPaneSize({ "Left", 5 }) },
      { key = "J", mods = "LEADER|SHIFT", action = act.AdjustPaneSize({ "Down", 5 }) },
      { key = "K", mods = "LEADER|SHIFT", action = act.AdjustPaneSize({ "Up", 5 }) },
      { key = "L", mods = "LEADER|SHIFT", action = act.AdjustPaneSize({ "Right", 5 }) },
      { key = "x", mods = "LEADER",       action = act.CloseCurrentPane({ confirm = true }) },

      -- Paste
      { key = "]", mods = "LEADER",       action = act.PasteFrom("Clipboard"),                           desc = "Paste from clipboard" },

      -- Key tables
      { key = "[", mods = "LEADER",       action = act.ActivateCopyMode,                                 desc = "Activate Copy mode" },
      { key = "/", mods = "LEADER",       action = act.Search({ CaseInSensitiveString = "" }),           desc = "Activate Search mode" },
      { key = ".", mods = "LEADER",       action = act.PaneSelect({ mode = "Activate" }) },
      { key = ",", mods = "LEADER|SHIFT", action = act.PaneSelect({ mode = "SwapWithActive" }) },
      {
        key = "r",
        mods = "LEADER",
        action = act.ActivateKeyTable({ name = "resize_pane", replace_current = true, one_shot = false }),
        desc = "Activate Resize mode"
      },

      -- Commands
      {
        action = H.search_pattern_preselect,
        desc = "Search with preset"
      },
    },
    key_tables = {
      copy_mode = {
        { key = "y",      mods = "NONE",  action = act.Multiple({ act.CopyTo("Clipboard"), act.ClearSelection, act.CopyMode('ClearSelectionMode') }), },
        { key = "v",      mods = "NONE",  action = act.CopyMode { SetSelectionMode = "Cell" } },
        { key = "v",      mods = "SHIFT", action = act.CopyMode { SetSelectionMode = "Line" } },
        { key = "v",      mods = "CTRL",  action = act.CopyMode { SetSelectionMode = "Block" } },
        { key = "h",      mods = "NONE",  action = act.CopyMode("MoveLeft") },
        { key = "j",      mods = "NONE",  action = act.CopyMode("MoveDown") },
        { key = "k",      mods = "NONE",  action = act.CopyMode("MoveUp") },
        { key = "l",      mods = "NONE",  action = act.CopyMode("MoveRight") },
        { key = "w",      mods = "NONE",  action = act.CopyMode("MoveForwardWord") },
        { key = "b",      mods = "NONE",  action = act.CopyMode("MoveBackwardWord") },
        { key = "e",      mods = "NONE",  action = act.CopyMode("MoveForwardWordEnd") },
        { key = "0",      mods = "NONE",  action = act.CopyMode("MoveToStartOfLine") },
        { key = "$",      mods = "SHIFT", action = act.CopyMode("MoveToEndOfLineContent") },
        { key = "^",      mods = "SHIFT", action = act.CopyMode("MoveToStartOfLineContent") },
        { key = "G",      mods = "NONE",  action = act.CopyMode("MoveToScrollbackBottom") },
        { key = "g",      mods = "NONE",  action = act.CopyMode("MoveToScrollbackTop") },
        { key = "h",      mods = "SHIFT", action = act.CopyMode("MoveToViewportTop") },
        { key = "m",      mods = "SHIFT", action = act.CopyMode("MoveToViewportMiddle") },
        { key = "l",      mods = "SHIFT", action = act.CopyMode("MoveToViewportBottom") },
        { key = "b",      mods = "CTRL",  action = act.ScrollByPage(-1.0) },
        { key = "u",      mods = "CTRL",  action = act.ScrollByPage(-0.5) },
        { key = "f",      mods = "CTRL",  action = act.ScrollByPage(1.0) },
        { key = "d",      mods = "CTRL",  action = act.ScrollByPage(0.5) },

        { key = "/",      mods = "NONE",  action = H.search_forward },
        { key = "?",      mods = "NONE",  action = H.search_backward },
        { key = "n",      mods = "NONE",  action = H.next_match },
        { key = "N",      mods = "NONE",  action = H.prior_match },

        { key = "Escape", mods = "NONE",  action = H.clear_selection_or_close },
      },
      search_mode = {
        {
          key = "Enter",
          mods = "NONE",
          action = act.Multiple({
            act.CopyMode("AcceptPattern"),
            act.ClearSelection,
            act.CopyMode("ClearSelectionMode"),
          }),
        },
        { key = "Escape", mods = "NONE", action = act.PopKeyTable },
      },
      resize_pane = {
        { key = "l",      mods = "NONE", action = act.AdjustPaneSize({ 'Right', 5 }) },
        { key = "h",      mods = "NONE", action = act.AdjustPaneSize({ 'Left', 5 }) },
        { key = "k",      mods = "NONE", action = act.AdjustPaneSize({ 'Up', 2 }) },
        { key = "j",      mods = "NONE", action = act.AdjustPaneSize({ 'Down', 2 }) },

        { key = 'Escape', mods = "NONE", action = act.PopKeyTable },
      }
    }
  }
})


return config
