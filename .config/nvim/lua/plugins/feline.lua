vim.o.termguicolors = true

local window_colors = require("colors").window
local syntax_colors = require("colors").syntax

local force_inactive = {
  filetypes = {
    "^NvimTree$",
    "^packer$",
    "^startify$",
    "^fugitive$",
    "^fugitiveblame$",
    "^help$",
  },
  buftypes = {
    "^terminal$",
  },
  bufnames = {},
}

local disable = {
  filetypes = {
    "^aerial$",
    "^qf$",
    "^neo.tree$",
  },
}

local colors = {
  bg = window_colors.bg0,
  fg = window_colors.white.base,
  black = window_colors.bg1,
  gray = "#161d27",
  white = window_colors.white.base,
  yellow = window_colors.yellow.base,
  cyan = syntax_colors.cyan.base,
  blue = window_colors.blue.base,
  oceanblue = window_colors.cyan.bright,
  green = window_colors.green.base,
  orange = window_colors.yellow.base,
  violet = window_colors.magenta.bright,
  magenta = window_colors.magenta.base,
  skyblue = window_colors.blue.base,
  red = window_colors.red.base,
}

local M = {
  winbar = {
    active = {},
    inactive = {},
  },
}

local build_statusline = function()
  local vi_mode_utils = require("feline.providers.vi_mode")

  local statusline = {
    active = { {}, {}, {} },
    inactive = { {}, {} },
  }

  statusline.active[1] = {
    {
      provider = "",
      hl = function()
        return {
          name = vi_mode_utils.get_mode_highlight_name(),
          fg = vi_mode_utils.get_mode_color(),
          style = "bold",
        }
      end,
      left_sep = {
        "slant_left_2",
      },
    },
    {
      provider = "file_type",
      hl = {
        fg = "white",
        bg = "blue",
      },
      left_sep = {
        "slant_left_2",
        {
          str = " ",
          hl = {
            bg = "blue",
            fg = "NONE",
          },
        },
      },
      right_sep = {
        { str = " ", hl = { bg = "blue", fg = "NONE" } },
        "slant_right",
      },
    },
    {
      provider = "file_info",
      opts = {
        type = "relative",
      },
      hl = {
        fg = "white",
        bg = "blue",
        style = "bold",
      },
      left_sep = {
        "slant_left_2",
        { str = " ", hl = { bg = "blue", fg = "NONE" } },
      },
      right_sep = {
        { str = " ", hl = { bg = "blue", fg = "NONE" } },
        "slant_right",
        " ",
      },
    },
    {
      provider = "position",
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_left_2_thin",
          hl = {
            fg = "fg",
            bg = "bg",
          },
        },
      },
    },
    {
      provider = "search_count",
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_left_2_thin",
          hl = {
            fg = "fg",
            bg = "bg",
          },
        },
      },
    },
    {
      provider = "diagnostic_errors",
      hl = { fg = "red" },
    },
    {
      provider = "diagnostic_warnings",
      hl = { fg = "yellow" },
    },
    {
      provider = "diagnostic_hints",
      hl = { fg = "cyan" },
    },
    {
      provider = "diagnostic_info",
      hl = { fg = "skyblue" },
    } }
  statusline.active[2] = {
    {
      provider = "git_branch",
      hl = {
        fg = "yellow",
        bg = "bg",
        style = "bold",
      },
    },
    {
      provider = "git_diff_added",
      hl = {
        fg = "green",
      },
    },
    {
      provider = "git_diff_changed",
      hl = {
        fg = "orange",
      },
    },
    {
      provider = "git_diff_removed",
      hl = {
        fg = "red",
      },
    },
  }

  statusline.active[3] = {
    {
      provider = "lsp_client_names",
      hl = { fg = "yellow", style = "bold" },
      right_sep = {
        " ",
        {
          str = "slant_right_2_thin",
          hl = { fg = "fg", bg = "bg" },
        },
      },
    },
    {
      provider = "file_encoding",
      hl = {
        style = "bold",
      },
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_right_2_thin",
          hl = { fg = "fg", bg = "bg" },
        },
      },
    },
    {
      provider = "file_size",
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_right_2_thin",
          hl = { fg = "fg", bg = "bg" },
        },
        " ",
      },
    },
    {
      provider = "line_percentage",
      hl = {
        style = "bold",
      },
      right_sep = " ",
    },
  }

  statusline.inactive[1] = {
    {
      provider = "",
      hl = function()
        return {
          fg = "white",
          style = "bold",
        }
      end,
      left_sep = {
        "slant_left_2",
      },
    },
    {
      provider = "file_type",
      hl = {
        fg = "white",
        bg = "blue",
      },
      left_sep = {
        "slant_left_2",
        {
          str = " ",
          hl = {
            bg = "blue",
            fg = "NONE",
          },
        },
      },
      right_sep = {
        { str = " ", hl = { bg = "blue", fg = "NONE" } },
        "slant_right",
      },
    },
    {
      provider = "file_info",
      hl = {
        fg = "white",
        bg = "blue",
        style = "bold",
      },
      left_sep = {
        "slant_left_2",
        { str = " ", hl = { bg = "blue", fg = "NONE" } },
      },
      right_sep = {
        { str = " ", hl = { bg = "blue", fg = "NONE" } },
        "slant_right",
        " ",
      },
    },
    {
      provider = "position",
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_left_2_thin",
          hl = {
            fg = "fg",
            bg = "bg",
          },
        },
      },
    },
  }

  statusline.inactive[2] = {
    {
      provider = "file_encoding",
      hl = {
        style = "bold",
      },
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_right_2_thin",
          hl = { fg = "fg", bg = "bg" },
        },
      },
    },
    {
      provider = "file_size",
      left_sep = " ",
      right_sep = {
        " ",
        {
          str = "slant_right_2_thin",
          hl = { fg = "fg", bg = "bg" },
        },
        " ",
      },
    },
    {
      provider = "line_percentage",
      hl = {
        style = "bold",
      },
      right_sep = " ",
    },
  }

  return statusline
end

M.winbar.active[1] = {
  {
    provider = " ",
    left_sep = "circle",
    hl = {
      fg = "green",
      bg = "bg",
    },
  },
  {
    provider = "file_info",
    opts = {
      type = "unique",
    },
    hl = {
      fg = "white",
      bg = "bg",
      style = "bold",
    },
  },
  {
    provider = " ",
    hl = {
      fg = "NONE",
      bg = "bg",
    },
  },


}

M.winbar.inactive[1] = {
  {
    provider = " ",
    right_sep = "circle",
    hl = {
      fg = "gray",
      bg = "bg",
    },
  },
  {
    provider = "file_info",
    opts = {
      type = "unique",
    },
    hl = {
      fg = "white",
      bg = "bg",
      style = "bold",
    },
  },
  {
    provider = " ",
    hl = {
      fg = "NONE",
      bg = "bg",
    },
  },
}

return {
  {
    "freddiehaddad/feline.nvim",
    config = function()
      require("feline").setup({
        theme = colors,
        components = build_statusline(),
        default_bg = window_colors.bg0,
        default_fg = window_colors.white.base,
        disable = disable,
        force_inactive = force_inactive,
      })

      require("feline").winbar.setup({
        theme = colors,
        components = M.winbar,
        default_bg = window_colors.bg0,
        default_fg = window_colors.white.base,
        disable = disable,
        force_inactive = force_inactive,
      })
    end,
  },
}
