local wezterm = require("wezterm")
local act = wezterm.action

local M = {}

---@class ListSelectOpts
---@field window   _.wezterm.Window
---@field pane     _.wezterm.Pane
---@field title    string
---@field choices  {id : string, label: string}[]
---@field callback fun(window: _.wezterm.Window, pane: _.wezterm.Pane, id: string)

---@param opts ListSelectOpts
M.list_select = function(opts)
  opts.window:perform_action(
    act.InputSelector({
      title = opts.title,
      choices = opts.choices,
      action = wezterm.action_callback(function(win1, pane1, id)
        if not id then
          return
        end
        opts.callback(win1, pane1, id)
      end),
    }),
    opts.pane
  )
end

---@class PromptInputOpts
---@field window   _.wezterm.Window
---@field pane     _.wezterm.Pane
---@field prompt   string
---@field callback fun(window: _.wezterm.Window, pane: _.wezterm.Pane, id: string)

---@param opts PromptInputOpts
M.prompt_input = function(opts)
  opts.window:perform_action(
    act.PromptInputLine({
      description = opts.prompt,
      action = wezterm.action_callback(function(window, pane, input)
        if not input or #input == 0 then
          return
        end
        opts.callback(window, pane, input)
      end),
    }),
    opts.pane
  )
end

---@class SwitchWorkspaceOpts
---@field window _.wezterm.Window
---@field pane   _.wezterm.Pane
---@field name   string
---@field cwd    string|nil

---@param opts   SwitchWorkspaceOpts
M.switch_workspace = function(opts)
  local ws = {
    name = opts.name,
  }
  if opts.cwd ~= nil then
    ws.spawn = { cwd = opts.cwd }
    opts.window:perform_action(act.SwitchToWorkspace(ws), opts.pane)
  end
end

return M
