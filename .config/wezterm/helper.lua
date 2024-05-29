local wezterm = require("wezterm")
local act = wezterm.action

local M = {}

M.show_unicode_select = wezterm.action_callback(function(window, pane)
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

return M
