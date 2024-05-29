local wezterm = require("wezterm")
local util = require("util")
local mux = wezterm.mux
local colors = require("custom.theme").colors
local H = require("helper")

local M = {}

---@class Label
---@field name   string
---@field id     string
---@field active boolean
---@field exists boolean

---@param labels Label[]
---@return { id: string, label: string }[]
local format_labels = function(labels)
  local elements = {}

  for _, label in ipairs(labels) do
    local elem = {}
    if label.active then
      table.insert(elem, { Foreground = { Color = colors.ansi[3] } })
      table.insert(elem, { Text = "" })
      table.insert(elem, { Foreground = { Color = colors.foreground } })
      table.insert(elem, { Text = " " })
    elseif label.exists then
      table.insert(elem, { Foreground = { Color = colors.ansi[4] } })
      table.insert(elem, { Text = "" })
      table.insert(elem, { Foreground = { Color = colors.foreground } })
      table.insert(elem, { Text = " " })
    elseif label.id == "new" then
      table.insert(elem, { Foreground = { Color = colors.ansi[5] } })
      table.insert(elem, { Text = "󰎜" })
      table.insert(elem, { Foreground = { Color = colors.foreground } })
      table.insert(elem, { Text = " " })
    else
      table.insert(elem, { Foreground = { Color = colors.foreground } })
      table.insert(elem, { Text = "  " })
    end

    local id
    if label.exists then
      id = "T" .. label.id
    else
      id = "F" .. label.id
    end

    table.insert(elem, { Text = label.name })
    table.insert(elements, { id = id, label = wezterm.format(elem) })
  end

  return elements
end

M.select = wezterm.action_callback(function(window, pane)
  local home = wezterm.home_dir
  local choices = {}

  local active_workspace = mux.get_active_workspace()
  for _, workspace in ipairs(mux.get_workspace_names()) do
    table.insert(choices, {
      id = workspace,
      exists = true,
      active = workspace == active_workspace,
      name = workspace,
    })
  end

  -- Create new
  table.insert(choices, { id = "new", exists = false, active = false, name = "Create new" })

  -- zoxide
  local suc, stdout = wezterm.run_child_process({ "zoxide", "query", "-l" })

  if suc then
    for _, row in ipairs(wezterm.split_by_newlines(stdout)) do
      table.insert(choices, {
        id = row,
        name = row:gsub(wezterm.home_dir, "~"),
        active = false,
        exists = false,
      })
    end
  end

  util.list_select({
    window = window,
    pane = pane,
    title = "Select WS",
    choices = format_labels(choices),
    callback = function(win1, pane1, eid)
      local exists
      if string.sub(eid, 1, 1) == "T" then
        exists = true
      else
        exists = false
      end
      local id = string.sub(eid, 2)

      if id == "new" then
        util.prompt_input({
          window = win1,
          pane = pane1,
          prompt = "Enter workspace name:",
          callback = function(win2, pane2, name)
            util.switch_workspace({
              window = win2,
              pane = pane2,
              name = name,
              cwd = home,
            })
          end,
        })
      else
        local name
        if exists then
          name = id
        else
          name = H.basename(id)
        end
        util.switch_workspace({
          window = win1,
          pane = pane1,
          name = name,
          cwd = id,
        })
      end
    end,
  })
end)

return M
