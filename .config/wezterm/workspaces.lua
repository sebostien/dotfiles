local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action

local workspaces = {
	dotfiles = function(window, pane)
		local tab_1, pane_1, proj_window = mux.spawn_window({
			workspace = "dotfiles",
			cwd = "/home/sn/",
		})

		tab_1:set_title("home")
		pane_1:send_text("config s\r")

		local tab_2, _, _ = proj_window:spawn_tab({ cwd = "/home/sn/.config/nvim" })
		tab_2:set_title("nvim")

		local tab_3, _, _ = proj_window:spawn_tab({ cwd = "/home/sn/.config/xmonad" })
		tab_3:set_title("xmonad")

		mux.set_active_workspace("dotfiles")
	end,
	TME290 = function(window, pane)
		local _, first_pane, _ = mux.spawn_window({
			workspace = "TME290",
			cwd = "/home/sn/Documents/Chalmers/kurser/TME290/",
		})

		first_pane:send_text("task project:TME290\r")
		mux.set_active_workspace("TME290")
	end,
	EDA491 = function(window, pane)
		local _, first_pane, _ = mux.spawn_window({
			workspace = "EDA491",
			cwd = "/home/sn/Documents/Chalmers/kurser/EDA491/",
		})

		first_pane:send_text("task project:EDA491\r")
		mux.set_active_workspace("EDA491")
	end,
}

return function(window, pane)
	local ws = {}
	local prompt = {}

	local i = 0

	local active = wezterm.mux.get_active_workspace()
	for _, k in ipairs(wezterm.mux.get_workspace_names()) do
		i = i + 1
		if active == k then
			table.insert(prompt, { Foreground = { AnsiColor = "Green" } })
		else
			table.insert(prompt, { Foreground = { AnsiColor = "White" } })
		end
		table.insert(prompt, { Text = " " .. i .. ": " .. k .. "\n" })
		ws[i] = k
		ws[string.lower(k)] = k
	end

	for k, v in pairs(workspaces) do
		if ws[string.lower(k)] == nil then
			i = i + 1
			table.insert(prompt, { Foreground = { Color = "#6F7B87" } })
			table.insert(prompt, { Text = " " .. i .. ": " .. k .. "\n" })
			ws[i] = v
			ws[string.lower(k)] = v
		end
	end

	table.insert(prompt, { Attribute = { Intensity = "Bold" } })
	table.insert(prompt, { Foreground = { AnsiColor = "Blue" } })
	table.insert(prompt, { Text = "Enter number or name for workspace:" })

	window:perform_action(
		act.PromptInputLine({
			description = wezterm.format(prompt),
			action = wezterm.action_callback(function(new_window, new_pane, line)
				if line then
					local workspace = ws[tonumber(line)] or ws[string.lower(line)] or line

					if type(workspace) == "function" then
						workspace(new_window, new_pane)
					elseif type(workspace) == "string" then
						window:perform_action(
							act.SwitchToWorkspace({
								name = workspace,
							}),
							new_pane
						)
					end
				end
			end),
		}),
		pane
	)
end
