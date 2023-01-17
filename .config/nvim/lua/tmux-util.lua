local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local conf = require("telescope.config").values
local settings = require("settings")

local M = {}

M.new_session = function(opts)
  opts = opts or {}
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
      attach_mappings = function(promt_bufnr, map)
        actions.select_default:replace(function()
          actions.close(promt_bufnr)
          local selection = action_state.get_selected_entry()
          -- TODO: Open selection dir in tmux. Validate also
          vim.notify(selection[1], vim.log.levels.INFO)
        end)
        return true
      end,
    })
    :find()
end

return M
