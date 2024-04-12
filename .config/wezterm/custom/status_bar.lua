local wezterm = require("wezterm")
local colors = require("custom.theme").colors
local H = require("helper")

local BG0 = colors.background
local BLUE = colors.ansi[5]

local M = {}

--  
--  
local TRIANGLE_U_L = utf8.char(0xE0BC)        -- 
local TRIANGLE_U_R = utf8.char(0xE0BE)        -- 
local TRIANGLE_D_L = utf8.char(0xE0B8)        -- 
local TRIANGLE_D_R = utf8.char(0xE0BA)        -- 
local ARROW = " " .. utf8.char(0xF460) .. " " -- 

---@param window _.wezterm.TabInformation|_.wezterm.Window
---@param pane _.wezterm.Pane
local show_key_table = function(window, pane)
  local key_table = window:active_key_table()
  if not key_table then
    key_table = "normal"
  end

  local backgrounds = {
    normal = colors.ansi[3],
    copy_mode = colors.ansi[4],
    search_mode = colors.ansi[6],
    resize_pane = colors.ansi[2],
  }

  local bg = backgrounds[key_table] or colors.ansi[5]

  window:set_left_status(wezterm.format({
    { Background = { Color = BG0 } },
    { Foreground = { Color = BG0 } },
    { Text = " " },
    { Background = { Color = BG0 } },
    { Foreground = { Color = bg } },
    { Text = TRIANGLE_U_R },
    { Background = { Color = bg } },
    { Foreground = { Color = BG0 } },
    { Text = "  " .. TRIANGLE_U_R }
  }))
end

---@class Cell
---@field text string
---@field bg string
---@field fg string|nil

---@param window _.wezterm.TabInformation|_.wezterm.Window
---@param pane _.wezterm.Pane
---@param cells Cell[]
local build_status = function(window, pane, cells)
  local elements = {}

  for _, cell in ipairs(cells) do
    table.insert(elements, { Background = { Color = BG0 } })
    table.insert(elements, { Foreground = { Color = cell.bg } })
    table.insert(elements, { Text = " " .. TRIANGLE_D_R })
    table.insert(elements, { Foreground = { AnsiColor = cell.fg or "White" } })
    table.insert(elements, { Background = { Color = cell.bg } })
    table.insert(elements, { Text = " " .. cell.text .. " " })

    table.insert(elements, { Foreground = { Color = BG0 } })
    table.insert(elements, { Background = { Color = cell.bg } })
    table.insert(elements, { Text = TRIANGLE_D_R })
  end

  -- Black space on right side
  table.insert(elements, { Foreground = { Color = BG0 } })
  table.insert(elements, { Background = { Color = BG0 } })
  table.insert(elements, { Text = " " })

  window:set_right_status(wezterm.format(elements))
  show_key_table(window, pane)
end


---@param _ _.wezterm.ConfigBuilder
M.apply_to_config = function(_)
  wezterm.on("update-status", function(window, pane)
    local cwd_url = pane:get_current_working_dir()
    local cwd = "~"
    if cwd_url ~= nil then
      cwd = cwd_url.file_path
    end

    ---@type Cell[]
    local cells = {
      { text = cwd,                             bg = BLUE },
      { text = wezterm.mux.get_domain():name(), bg = BLUE },
      { text = window:active_workspace(),       bg = BLUE }
    }

    build_status(window, pane, cells)
  end)

  wezterm.on("format-tab-title", function(tab, _, _, _, _, _)
    local tab_title = function(tab_info)
      local title = tab_info.tab_title
      local pane_title = tab.active_pane.title
      -- if the tab title is explicitly set, take that
      if title and #title > 0 then
        return title
      elseif pane_title and #pane_title > 0 then
        return pane_title
      else
        return title .. H.basename(pane_title.foreground_process_name)
      end
    end

    local title = tab.tab_index + 1 .. ARROW .. tab_title(tab)

    local bg = BG0
    if tab.is_active then
      bg = BLUE
    end
    return {
      { Foreground = { Color = BG0 } },
      { Background = { Color = bg } },
      { Text = TRIANGLE_D_L },
      { Foreground = { Color = "White" } },
      { Background = { Color = bg } },
      { Text = " " .. title .. " " },
      { Foreground = { Color = bg } },
      { Background = { Color = BG0 } },
      { Text = TRIANGLE_D_L },
    }
  end)
end

return M
