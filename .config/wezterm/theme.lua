return function(config)
  config.color_scheme = "nightfox"
  config.colors = {
    background = "#0D1117",
    foreground = "#FAFAFA",

    cursor_bg = "#999999",
    cursor_fg = "#FFFFFF",
    cursor_border = "#A5A5A5",

    selection_fg = "#FFFFFF",
    selection_bg = "#485460",

    scrollbar_thumb = "#222222",
    split = "#0d1117",

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

    -- Arbitrary colors of the palette in the range from 16 to 255
    -- indexed = { [136] = "#af8700" },

    tab_bar = {
      background = "#0D1117",

      active_tab = {
        bg_color = "#2880FE",
        fg_color = "#FFFFFF",
      },

      inactive_tab = {
        bg_color = "#0D1117",
        fg_color = "#FFFFFF",
      },

      -- Disable hover
      inactive_tab_hover = {
        bg_color = "#0D1117",
        fg_color = "#FFFFFF",
      },
    },
  }

  config.show_new_tab_button_in_tab_bar = false
  config.use_fancy_tab_bar = false
  config.tab_bar_at_bottom = true
  config.window_padding = {
    left = "4px",
    right = "4px",
    top = "10px",
    bottom = "5px",
  }

  config.command_palette_bg_color = "#1D2127"
  config.command_palette_fg_color = "#FFFFFF"
end
