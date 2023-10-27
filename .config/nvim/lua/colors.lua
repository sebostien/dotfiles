local M = {}

local x = {
  '#01060e',
  '#0a0e14',
  '#0d1016',
  '#161f2a',
  '#121922',
  '#191e25',
  '#1d2936',
  '#242831',
  '#292d36',
  '#2e323c',
  '#3d424d',
  '#626a73',
  '#b3b1ad',
  '#ffffff',
  '#e1af4b',
  '#ffb454',
  '#e6b450',
  '#f9af4f',
  '#f29668',
  '#fae994',
  '#ffee99',
  '#ff3333',
  '#d96c75',
  '#ea6c73',
  '#f07178',
  '#91b362',
  '#90e1c6',
  '#c7c7c7',
  '#686868',
  '#c2d94c',
  '#6994bf',
  '#39bae6',
  '#53bdfa',
  '#59c2ff',
  '#95e6cb',
}

M.palette = {
  black   = {
    base = "#0D1117",
    bright = "#272B31",
  },
  red     = {
    base = "#f44336",
    bright = "#ff5252",
    syntax = "#ff6c6b"
  },
  green   = {
    base = "#27ae60",
    bright = "#2ecc71",
    syntax = "#98be65"
  },
  yellow  = {
    base = "#FBC02D",
    bright = "#ecbe7b",
    syntax = "#986801",
  },
  blue    = {
    base = "#2880fe",
    bright = "#03A9F4",
    syntax = "#4078f2"
  },
  magenta = {
    base = "#9C27B0",
    bright = "#EA80FC",
    syntax = "#c678dd",
  },
  cyan    = {
    base = "#00c8d4",
    bright = "#34e7e4",
    syntax = "#79c0ff",
  },
  white   = {
    base = "#FAFAFA",
    bright = "#FFFFFF",
    syntax = "#FAFAFA",
  },
  orange  = {
    base = "#fa951a",
    bright = "#da8548",
    syntax = "#da8548"
  },
  pink    = {
    base   = "#c678dd",
    bright = "#ffafd2",
    syntax = "#ffafd2",
  },

  comment = "#6B6E74",

  bg0     = "#0D1117",
  bg1     = "#0D1117",
  bg2     = "#1A1E24",
  bg3     = "#272B31",
  bg4     = "#34383E",
  fg0     = "#EFEFEF",
  fg1     = "#C3C4C6",
  fg2     = "#97999D",
  fg3     = "#6B6E74",

  sel0    = "#34383E", -- Popup bg, visual selection bg
  sel1    = "#3F4349", -- Popup sel bg, search bg
}

M.window = {
  bg0 = "#0d1117",
  bg1 = "#13171d",

  black = {
    base = "#0d1117",
    bright = "#4a576f",
  },
  white = {
    base = "#fafafa",
    bright = "#ffffff",
  },
  red = {
    base = "#F44336",
    bright = "#ff5252",
  },
  green = {
    base = "#27ae60",
    bright = "#2ecc71",
  },
  yellow = {
    base = "#Fa951a",
    bright = "#FBC02D",
  },
  blue = {
    base = "#2880fe",
    bright = "#03A9F4",
  },
  magenta = {
    base = "#9C27B0",
    bright = "#EA80FC",
  },
  cyan = {
    base = "#00c8d4",
    bright = "#34e7e4",
  },
}

M.syntax = {
  red = {
    base = "#ff6c6b",
    bright = "#e45649",
  },
  green = {
    base = "#98be65",
    bright = "#50a14f",
  },
  orange = {
    base = "#da8548",
    bright = "#da8548",
  },
  yellow = {
    base = "#ecbe7b",
    bright = "#986801",
  },
  blue = {
    base = "#51afef",
    bright = "#4078f2",
  },
  magenta = {
    base = "#c678dd",
    bright = "#a626a4",
  },
  cyan = {
    base = "#0184bc",
    bright = "#46d9ff",
  },
}

M.window = {
  bg1 = "#13171d",
  bg0 = "#0d1117",

  black = {
    base = "#0d1117",
    bright = "#4a576f",
  },
  white = {
    base = "#fafafa",
    bright = "#ffffff",
  },
  red = {
    base = "#F44336",
    bright = "#ff5252",
  },
  green = {
    base = "#27ae60",
    bright = "#2ecc71",
  },
  yellow = {
    base = "#Fa951a",
    bright = "#FBC02D",
  },
  blue = {
    base = "#2880fe",
    bright = "#03A9F4",
  },
  magenta = {
    base = "#9C27B0",
    bright = "#EA80FC",
  },
  cyan = {
    base = "#00c8d4",
    bright = "#34e7e4",
  },
}

M.syntax = {
  red = {
    base = "#ff6c6b",
    bright = "#e45649",
  },
  green = {
    base = "#98be65",
    bright = "#50a14f",
  },
  orange = {
    base = "#da8548",
    bright = "#da8548",
  },
  yellow = {
    base = "#ecbe7b",
    bright = "#986801",
  },
  blue = {
    base = "#51afef",
    bright = "#4078f2",
  },
  magenta = {
    base = "#c678dd",
    bright = "#a626a4",
  },
  cyan = {
    base = "#0184bc",
    bright = "#46d9ff",
  },
}

return M
