return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-neotest/nvim-nio",
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "neovim/nvim-lspconfig",
    },
    ---@class DapOpts
    opts = {},
    ---@param opts DapOpts
    config = function(_, opts)
      local dap = require("dap")
      local ui = require("dapui")

      require("mason").setup()
      require("dapui").setup()
      require("nvim-dap-virtual-text").setup({})

      local js_debug_adapter = vim.fn.exepath("js-debug-adapter")
      if js_debug_adapter ~= "" then
        dap.adapters["pwa-node"] = {
          type = "server",
          host = "::1",
          port = "12999",
          executable = {
            command = js_debug_adapter,
            args = { "12999" },
          },
        }

        dap.configurations.javascript = {
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch File",
            program = "${file}",
            cwd = "${workspaceFolder}",
          },
        }
      end

      dap.listeners.before.attach.dapui_config = function()
        ui.open()
      end
      dap.listeners.before.launch.dapui_config = function()
        ui.open()
      end
      dap.listeners.before.event_terminated.dapui_config = function()
        ui.close()
      end
      dap.listeners.before.event_exited.dapui_config = function()
        ui.close()
      end

      vim.keymap.set("n", "<F1>", dap.continue, { desc = "DAP: Continue" })
      vim.keymap.set("n", "<F2>", dap.step_over, { desc = "DAP: Step Over" })
      vim.keymap.set("n", "<F3>", dap.step_into, { desc = "DAP: Step Into" })
      vim.keymap.set("n", "<F4>", dap.step_out, { desc = "DAP: Step Out" })
      vim.keymap.set("n", "<F5>", dap.step_back, { desc = "DAP: Step Back" })
      vim.keymap.set("n", "<F11>", dap.restart, { desc = "DAP: Restart" })
      vim.keymap.set("n", "<F12>", dap.close, { desc = "DAP: Close" })

      vim.keymap.set("n", "<leader>b", dap.toggle_breakpoint, { desc = "DAP: Toggle Breakpoint" })
      vim.keymap.set("n", "<leader>gb", dap.run_to_cursor, { desc = "DAP: Run to Cursor" })
      vim.keymap.set("n", "<leader>gr", dap.repl.open, { desc = "DAP: Open Repl" })
      vim.keymap.set("n", "<leader>gh", require("dap.ui.widgets").hover, { desc = "DAP: Hover" })
      vim.keymap.set("n", "<leader>gp", require("dap.ui.widgets").preview, { desc = "DAP: Preview" })
      vim.keymap.set("n", "<leader>?", function()
        require("dapui").eval(nil, { enter = true })
      end, { desc = "DAP: Inspect under cursor" })
      vim.keymap.set("n", "<leader>gf", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.frames)
      end, { desc = "DAP: Float Frames" })
      vim.keymap.set("n", "<leader>gs", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.scopes)
      end, { desc = "DAP: Float Scopes" })
    end,
  },
}
