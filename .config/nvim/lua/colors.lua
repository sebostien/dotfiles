---@class Color
---@field bright string Very bright
---@field base string   Pretty bright
---@field mantle string Pretty Dark
---@field crust string  Very Dark

---@class SNColors
---@field bg string
---@field fg string
---@field accent string
---@field black Color
---@field white Color
---@field red Color
---@field green Color
---@field yellow Color
---@field blue Color
---@field purple Color
local COLORS = {
  bg = "#0D1117",
  fg = "#F8FAFC",
  accent = "#2ecc71",
  black = {
    bright = "#222934",
    base = "#1c222b", -- TODO: Change this
    mantle = "#151a22",
    crust = "#0D1117",
  },
  white = {
    bright = "#F8FAFC",
    base = "#E2E5E7",
    mantle = "#CCCFD2",
    crust = "#A0A3A7",
  },
  red = {
    bright = "#F29D9D",
    base = "#F97878",
    mantle = "#FF5252",
    crust = "#F44336",
  },
  green = {
    -- bright = "#E0E7CD",
    bright = "#BAE67E",
    -- base =  "#A6CC70",
    base = "#98Be65",
    mantle = "#50A14F",
    -- mantle = "#2Ecc71",
    crust = "#27AE60",
  },
  yellow = {
    base = "#FFD580",
    bright = "#FBC02D",
    mantle = "#FA951A",
    crust = "#FF8F40",
  },
  blue = {
    -- bright = "#95E6CB",
    -- bright = "#90E1C6",
    bright = "#34e7e4",
    base = "#4cd4bd",
    -- base = "#4CBF99",
    mantle = "#03a9F4",
    crust = "#2880FE",
    -- "#39BAE6",
    -- "#53BDFA",
    -- "#59C2FF",
    -- "#61BDFF",
    -- "#5CCFE6",
    -- "#73D0FF",
  },
  purple = {
    bright = "#FFAFD2",
    base = "#D3B8F9",
    mantle = "#AD6FF7",
    crust = "#9C27B0",
    -- "#EA80FC",
    -- "#C678DD",
    -- "#CB9FF8",
  },
}

local tmp = {

  "#4D5566",
  "#575F66",
  "#707A8C",
  "#B3B1AD",
  "#8A9199",
  "#CBCCC6",
  "#5C6773",
  "#626A73",
  "#313D37",
  "#3E373A",
  "#404755",
  "#5F687A",
  "#33415E",
  "#323A4C",
  "#232A4C",
  "#576070",
  "#383E4C",
  "#323945",
  "#464D5E",
  "#273747",
  "#304357",
  "#393F4D",
  "#607080",
  "#3E4B59",
  "#ABB0B6",
  "#EFF0F1",
  "#FFFFFF",
  "#CCCED0",
  "#F0F0F0",
  "#CDD0D3",
  "#A0A6AC",
  "#D1E4F4",
  "#E7E8E9",
  "#E1E1E2",
  "#D3D5D8",
  "#E6E7E9",
  "#828C99",
  "#C1C9E6",
  "#A3AAC2",
  "#8E94AB",
  "#686868",
  "#6B6E74",
  "#6B6E74",
}

local macchiato = {
  rosewater = "#F5B8AB",
  flamingo = "#F29D9D",
  pink = "#AD6FF7",
  mauve = "#FF8F40",
  red = "#E66767",
  maroon = "#EB788B",
  peach = "#FAB770",
  yellow = "#FACA64",
  green = "#70CF67",
  teal = "#4CD4BD",
  sky = "#61BDFF",
  sapphire = "#4BA8FA",
  blue = "#00BFFF",
  lavender = "#00BBCC",
  text = "#C1C9E6",
  subtext1 = "#A3AAC2",
  subtext0 = "#8E94AB",
  overlay2 = "#7D8296",
  overlay1 = "#676B80",
  overlay0 = "#464957",
  surface2 = "#3A3D4A",
  surface1 = "#2F313D",
  surface0 = "#1D1E29",
  base = "#0b0b12",
  mantle = "#11111a",
  crust = "#191926",
  rosewater = "#f5e0dc",
  flamingo = "#f2cdcd",
  mauve = "#cba6f7",
  pink = "#f5c2e7",
  base = "#1e1e2e",
  blue = "#89b4fa",
  crust = "#11111b",
  green = "#a6e3a1",
  lavender = "#b4befe",
  mantle = "#181825",
  maroon = "#eba0ac",
  overlay0 = "#6c7086",
  overlay1 = "#7f849c",
  overlay2 = "#9399b2",
  peach = "#fab387",
  red = "#f38ba8",
  sapphire = "#74c7ec",
  sky = "#89dceb",
  subtext0 = "#a6adc8",
  subtext1 = "#bac2de",
  surface0 = "#313244",
  surface1 = "#45475a",
  surface2 = "#585b70",
  teal = "#94e2d5",
  text = "#cdd6f4",
  yellow = "#f9e2af",
}

return COLORS
