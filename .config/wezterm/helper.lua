local wezterm = require("wezterm")
local act = wezterm.action
local mux = wezterm.mux

local M = {}

local BACKWARD = 0
local FORWARD = 1

wezterm.GLOBAL.search_direction = FORWARD

---@param s string
--- Equivalent to POSIX basename(3)
--- Given "/foo/bar" returns "bar"
--- Given "c:\\foo\\bar" returns "bar"
M.basename = function(s)
  return string.gsub(s, "(.*[/\\])(.*)", "%2")
end

M.clear_selection_or_close = wezterm.action_callback(function(window, pane)
  local action

  if window:get_selection_text_for_pane(pane) ~= "" then
    action = act.Multiple({
      act.ClearSelection,
      act.CopyMode("ClearSelectionMode"),
    })
  else
    wezterm.GLOBAL.search_direction = nil
    action = act.CopyMode("Close")
  end

  window:perform_action(action, pane)
end)

M.next_match = wezterm.action_callback(function(window, pane)
  local direction = wezterm.GLOBAL.search_direction
  local action

  if not direction then
    return
  end

  if direction == BACKWARD then
    action = act.Multiple({
      act.CopyMode("PriorMatch"),
      act.ClearSelection,
      act.CopyMode("ClearSelectionMode"),
    })
  elseif direction == FORWARD then
    action = act.Multiple({
      act.CopyMode("NextMatch"),
      act.ClearSelection,
      act.CopyMode("ClearSelectionMode"),
    })
  end

  window:perform_action(action, pane)
end)

M.prior_match = wezterm.action_callback(function(window, pane)
  local direction = wezterm.GLOBAL.search_direction
  local action

  if not direction then
    return
  end

  if direction == BACKWARD then
    action = act.Multiple({
      act.CopyMode("NextMatch"),
      act.ClearSelection,
      act.CopyMode("ClearSelectionMode"),
    })
  elseif direction == FORWARD then
    action = act.Multiple({
      act.CopyMode("PriorMatch"),
      act.ClearSelection,
      act.CopyMode("ClearSelectionMode"),
    })
  end

  window:perform_action(action, pane)
end)

M.move_pane_to_new_tab = wezterm.action_callback(function(_, pane)
  local tab, _ = pane:move_to_new_tab()
  tab:activate()
end)

M.rename_tab = wezterm.action_callback(function(window, pane)
  window:perform_action(act.PromptInputLine({
    description = "Rename tab: ",
    action = wezterm.action_callback(function(_, _, line)
      if line then
        window:active_tab():set_title(line)
      end
    end),
  }), pane)
end)


M.rename_workspace = wezterm.action_callback(function(window, pane)
  window:perform_action(act.PromptInputLine({
    description = "Rename workspace: ",
    action = wezterm.action_callback(function(_, _, line)
      if line then
        mux.rename_workspace(mux.get_active_workspace(), line)
      end
    end),
  }), pane)
end)

M.search_backward = wezterm.action_callback(function(window, pane)
  wezterm.GLOBAL.search_direction = BACKWARD

  window:perform_action(act.Multiple({
    act.CopyMode("ClearPattern"),
    act.CopyMode("EditPattern"),
  }), pane)
end)

M.search_forward = wezterm.action_callback(function(window, pane)
  wezterm.GLOBAL.search_direction = FORWARD

  window:perform_action(act.Multiple({
    act.CopyMode("ClearPattern"),
    act.CopyMode("EditPattern"),
  }), pane)
end)

M.show_unicode_select = wezterm.action_callback(function(window, pane)
  window:perform_action(act.InputSelector({
    title = "Select type",
    choices = {
      { id = "RecentlyUsed",      label = "Recently used" },
      { id = "UnicodeNames",      label = "All unicode" },
      { id = "NerdFonts",         label = "Nerd Fonts" },
      { id = "Flags",             label = "Flags" },
      { id = "SmileysAndEmotion", label = "Smileys" },
      { id = "PeopleAndBody",     label = "People" },
      { id = "AnimalsAndNature",  label = "Animals and Nature" },
      { id = "FoodAndDrink",      label = "Food and Drinks" },
      { id = "TravelAndPlaces",   label = "Travel and Places" },
      { id = "Activities",        label = "Activites" },
      { id = "Objects",           label = "Objects" },
      { id = "Symbols",           label = "Symbols" },
    },
    action = wezterm.action_callback(function(_, _, id, _)
      if id and #id > 0 then
        window:perform_action(act.CharSelect({
          copy_on_select = true,
          copy_to = "ClipboardAndPrimarySelection",
          group = id,
        }), pane)
      end
    end)
  }), pane)
end)

M.search_pattern_preselect = wezterm.action_callback(function(window, pane)
  window:perform_action(act.InputSelector({
    title = "Select preset",
    choices = {
      { id = "#[a-f0-9]{6,}", label = "Hex color" },
      { id = "#[a-f0-9]{6,}", label = "Hex color" },
      { id = "[a-f0-9]{6,}",  label = "Git Hash" },
      { id = "custom",        label = "Custom" },
    },
    action = wezterm.action_callback(function(_, _, id, _)
      if id == "custom" then
        window:perform_action(
          act.Search({ CaseInSensitiveString = "" }),
          pane
        )
      else
        window:perform_action(
          act.Search({
            Regex = id
          }),
          pane)
      end
    end)
  }), pane)
end)

return M
