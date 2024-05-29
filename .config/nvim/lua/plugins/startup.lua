local header = {
  '        ╻ ╻   ╻   ┏┳┓',
  '   NEO  ┃┏┛   ┃   ┃┃┃',
  '        ┗┛    ╹   ╹ ╹',
}

local layout = {
  { type = "padding", val = 1, },
  {
    type = "text",
    val = header,
    opts = {
      position = "center",
      hl = "DashboardHeader",
    },
  },
  { type = "padding", val = 1, },
  -- mru goes here
  { type = "padding", val = 1, },
  {
    type = "text",
    val = {},
    opts = {
      position = "center",
      hl = "DashboardFooter",
    },
  },
}

local update_layout = function()
  local nvim_version = vim.fn.split(vim.api.nvim_exec2("version", { output = true }).output, "\n")[1]
  local stats = require("lazy").stats()
  local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
  layout[#layout].val = {
    "  " .. nvim_version,
    "⚡ Loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms .. "ms"
  }
end

update_layout()

return {
  {
    'goolord/alpha-nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'nvim-lua/plenary.nvim'
    },
    config = function()
      local theta = require('alpha.themes.theta').config
      table.insert(layout, 4, theta.layout[4])

      require 'alpha'.setup({ layout = layout })

      vim.api.nvim_create_autocmd("User", {
        once = true,
        pattern = "LazyVimStarted",
        callback = function()
          update_layout()
          pcall(vim.cmd.AlphaRedraw)
        end,
      })
    end
  }
}
