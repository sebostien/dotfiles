local settings = require("settings")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local conf = require("telescope.config").values

local M = {}

M.telescope_cd = function()
  local opts = {}
  local dirs = table.concat(settings.project_dirs, " ")

  local pwd = vim.fn.getcwd()
  local all = vim.fn.system("find " .. dirs .. " " .. pwd .. " -mindepth 1 -maxdepth 1 -type d")
  local results = {}
  for e in string.gmatch(all, "([^\n]*)\n?") do
    table.insert(results, e)
  end

  pickers
      .new(opts, {
        prompt_title = "Select directory",
        finder = finders.new_table({
          results = results,
        }),
        sorter = conf.generic_sorter(opts),
        attach_mappings = function(promt_bufnr)
          actions.select_default:replace(function()
            actions.close(promt_bufnr)
            local selection = action_state.get_selected_entry()
            vim.api.nvim_set_current_dir(selection[1])
          end)
          return true
        end,
      })
      :find()
end

return M
