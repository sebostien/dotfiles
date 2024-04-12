local wezterm = require("wezterm")
local act = wezterm.action

local M = {}

---@class KeyBinding
---@field key  string|nil
---@field mods string|nil
---@field icon string|nil
---@field action _.wezterm._KeyAssignmentAction|_.wezterm._CallbackAction|nil
---@field desc string|nil

---@class KeyTables
---@field normal      KeyBinding[]
---@field key_tables table<string, _.wezterm.KeyBinding[]>

---@class Setup
---@field leader { key: string, mods: string, timeout_milliseconds: integer }
---@field keys KeyTables

---@param config _.wezterm.ConfigBuilder
---@param custom Setup
M.apply_to_config = function(config, custom)
  local command_palette_extra = {}

  config.keys = {}
  config.key_tables = {}

  config.leader = custom.leader
  -- Leader twice will input leader raw
  table.insert(config.keys,
    {
      key = custom.leader.key,
      mods = custom.leader.mods,
      action = act.SendKey({ key = custom.leader.key, mods = custom.leader.mods })
    })

  -- Insert keys
  config.key_tables = custom.keys.key_tables
  for _, v in ipairs(custom.keys.normal) do
    if v.key ~= nil then
      table.insert(config.keys,
        { key = v.key, mods = v.mods, action = v.action }
      )
    end

    -- Insert into command palette
    if v.desc ~= nil then
      local o = { brief = v.desc, action = v.action }
      if v.icon ~= nil then
        o.icon = v.icon
      end
      table.insert(command_palette_extra, o)
    end
  end

  wezterm.on("augment-command-palette", function() return command_palette_extra end)
end

return M
