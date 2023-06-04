local wezterm = require("wezterm")

local BG0 = "#0D1107"
local BLUE = "#2880FE"

--  
--  
local TRIANGLE_U_L = utf8.char(0xE0BC) -- 
local TRIANGLE_U_R = utf8.char(0xE0BE) -- 
local TRIANGLE_D_L = utf8.char(0xE0B8) -- 
local TRIANGLE_D_R = utf8.char(0xE0BA) -- 
local ARROW = utf8.char(0xF460) -- 

-- Equivalent to POSIX basename(3)
-- Given "/foo/bar" returns "bar"
-- Given "c:\\foo\\bar" returns "bar"
local function basename(s)
	return string.gsub(s, "(.*[/\\])(.*)", "%2")
end

return function()
	wezterm.on("update-status", function(window, pane)
		local cells = {}

		local cwd_uri = pane:get_current_working_dir()
		table.insert(cells, cwd_uri)
		if cwd_uri then
			cwd_uri = cwd_uri:sub(8)
			local slash = cwd_uri:find("/")
			local hostname = ""
			if slash then
				hostname = cwd_uri:sub(1, slash - 1)
				-- Remove the domain name portion of the hostname
				local dot = hostname:find("[.]")
				if dot then
					hostname = hostname:sub(1, dot - 1)
				end

				table.insert(cells, hostname)
			end
		end

		table.insert(cells, wezterm.mux.get_domain():name())

		table.insert(cells, window:active_workspace())

		local date = wezterm.strftime("%a %b %-d %H:%M")
		table.insert(cells, date)

		-- table.insert(cells, "|" .. window. .. "|")

		local elements = {}

		local function push(text, is_last)
			table.insert(elements, { Background = { Color = BG0 } })
			table.insert(elements, { Foreground = { Color = BLUE } })
			table.insert(elements, { Text = " " .. TRIANGLE_D_R })
			table.insert(elements, { Foreground = { AnsiColor = "White" } })
			table.insert(elements, { Background = { Color = BLUE } })
			table.insert(elements, { Text = " " .. text .. " " })

			table.insert(elements, { Foreground = { Color = BG0 } })
			table.insert(elements, { Background = { Color = BLUE } })
			table.insert(elements, { Text = TRIANGLE_D_R })

			if is_last then
				table.insert(elements, { Foreground = { Color = BG0 } })
				table.insert(elements, { Background = { Color = BG0 } })
				table.insert(elements, { Text = " " })
			end
		end

		while #cells > 0 do
			local cell = table.remove(cells, 1)
			push(cell, #cells == 0)
		end

		window:set_right_status(wezterm.format(elements))
	end)

	wezterm.on("format-tab-title", function(tab, _, _, _, _, _)
		local pane = tab.active_pane
		local title = tab.tab_index + 1 .. ARROW
		if pane.title then
			title = title .. pane.title
		else
			title = title .. basename(pane.foreground_process_name)
		end
		local bg = BG0
		if tab.is_active then
			bg = BLUE
		end
		return {
			{ Foreground = { Color = bg } },
			{ Background = { Color = BG0 } },
			{ Text = TRIANGLE_D_R },
			{ Foreground = { Color = "White" } },
			{ Background = { Color = bg } },
			{ Text = " " .. title .. " " },
			{ Foreground = { Color = bg } },
			{ Background = { Color = BG0 } },
			{ Text = TRIANGLE_D_L },
		}
	end)
end
