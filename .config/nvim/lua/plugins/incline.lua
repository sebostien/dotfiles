local function get_diagnostic_label(props)
  local icons = {
    Error = "",
    Warn = "",
    Info = "",
    Hint = "",
  }

  local label = {}
  for severity, icon in pairs(icons) do
    local n = #vim.diagnostic.get(props.buf, { severity = vim.diagnostic.severity[string.upper(severity)] })
    if n > 0 then
      table.insert(label, { icon .. " " .. n .. " ", group = "DiagnosticSign" .. severity })
    end
  end
  return label
end

return {
  {
    "b0o/incline.nvim",
    event = "BufReadPre",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      {
        "<leader>ti",
        function()
          require("incline").toggle()
        end,
        desc = "Toggle Incline",
      },
    },
    config = function()
      local helpers = require("incline.helpers")
      local devicons = require("nvim-web-devicons")

      require("incline").setup({
        window = {
          padding = 0,
          margin = { horizontal = 0 },
        },
        debounce_threshold = { falling = 500, rising = 250 },
        highlight = {
          groups = {
            -- InclineNormal = { default = true, group = "lualine_a_normal" },
            -- InclineNormalNC = { default = true, group = "Comment" },
          },
        },
        render = function(props)
          local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
          if filename == "" then
            filename = "[No Name]"
          end
          local diagnostics = get_diagnostic_label(props)
          local modified = vim.bo[props.buf].modified
          local ft_icon, ft_color = devicons.get_icon_color(filename)

          local buffer = {
            ft_icon and { " ", ft_icon, " ", guibg = ft_color, guifg = helpers.contrast_color(ft_color) } or "",
            { " " },
            { filename, gui = modified and "bold,italic" or "bold" },
            guibg = "#44406e",
          }

          if #diagnostics > 0 then
            table.insert(diagnostics, { "| ", guifg = "grey" })
          end
          for _, buffer_ in ipairs(buffer) do
            table.insert(diagnostics, buffer_)
          end
          return diagnostics
        end,
      })
    end,
  },
}
