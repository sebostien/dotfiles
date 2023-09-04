local lsp_keymaps = require("plugins.lsp.lsp_keymap")
local lsp_theme = require("plugins.lsp.lsp_theme")

-- "jose-elias-alvarez/null-ls.nvim", -- LSP config helper
-- "mfussenegger/nvim-dap", -- Debugger
-- DAP
-- {
--   "rcarriga/nvim-dap-ui",
--   dependencies = { "mfussenegger/nvim-dap" }
-- },

return {
  "j-hui/fidget.nvim", -- Show load status
  -----------------
  -- Eww widgets --
  {
    "elkowar/yuck.vim",
    ft = { "yuck" },
  },
  -----------
  -- Mason --
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    keys = { { "<localleader>lm", "<CMD>Mason<CR>", desc = "Mason" } },
    opts = {
      ui = {
        border = "rounded",
      },
    },
  },
  ----------
  -- Rust --
  {
    "simrat39/rust-tools.nvim",
    dependencies = {
      "neovim/nvim-lspconfig",
    },
    ft = { "rust" },
    config = function()
      local rt = require("rust-tools")
      rt.setup({
        tools = {
          executor = require("rust-tools.executors").termopen,
        },
        server = {
          on_attach = function(client, bufnr)
            lsp_theme()
            lsp_keymaps.on_attach(client, bufnr)
            -- Popup list from rt
            vim.keymap.set("n", "<Leader>ca", rt.code_action_group.code_action_group,
              lsp_keymaps.bufopts(bufnr, "Code actions"))
          end,
        },
      })
    end
  },
  {
    "saecki/crates.nvim",
    tag = 'v0.3.0',
    event = { "BufRead Cargo.toml" },
    config = true
  },
  ------------
  -- Python --
  {
    "wookayin/semshi",
    ft = "python",
    build = ":UpdateRemotePlugins",
    init = function()
      -- Disabled these features better provided by LSP or other more general plugins
      vim.g["semshi#error_sign"] = false
      vim.g["semshi#simplify_markup"] = false
      vim.g["semshi#mark_selected_nodes"] = false
      vim.g["semshi#update_delay_factor"] = 0.001

      -- This autocmd must be defined in init to take effect
      vim.api.nvim_create_autocmd({ "VimEnter", "ColorScheme" }, {
        group = vim.api.nvim_create_augroup("SemanticHighlight", {}),
        callback = function()
          -- Only add style, inherit or link to the LSP's colors
          vim.cmd([[
            highlight! semshiGlobal gui=italic
            highlight! semshiImported gui=bold
            highlight! link semshiParameter @lsp.type.parameter
            highlight! link semshiParameterUnused DiagnosticUnnecessary
            highlight! link semshiBuiltin @function.builtin
            highlight! link semshiAttribute @attribute
            highlight! link semshiSelf @lsp.type.selfKeyword
            highlight! link semshiUnresolved @lsp.type.unresolvedReference
            ]])
        end,
      })
    end,
  },
  --------------
  -- Markdown --
  {
    "iamcco/markdown-preview.nvim",
    build = "cd app && npm install",
    config = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
    ft = { "markdown" },
  },
  -------------
  -- Haskell --
  {
    "MrcJkb/haskell-tools.nvim",
    dependencies = {
      'nvim-telescope/telescope.nvim',
    },
    branch = "2.x.x",
    config = function()
      vim.g.haskell_tools = {
        hls = {
          on_attach = function(client, bufnr, _)
            lsp_theme()
            lsp_keymaps.on_attach(client, bufnr)
          end,
        }
      }
    end,
    ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
  },
  ----------------
  -- LSP-Config --
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim"
    },
    opts = {
      ensure_installed = {
        "lua_ls",
        "tsserver",
        "jsonls",
        "rust_analyzer",
        "eslint",
        "marksman",
        "texlab",
        "pyright",
      },
      automatic_installation = true,
    },
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "folke/trouble.nvim",
      ---------
      -- Lua --
      { "folke/neodev.nvim",    opts = {} },
      ----------
      -- JSON --
      { "b0o/SchemaStore.nvim", version = false },
    },
    keys = {
      { "<localleader>lr", "<CMD>LspRestart<CR><CMD>e<CR>",                       desc = "Restart lsp servers" },
      { "<localleader>ls", "<CMD>LspStop<CR><CMD>lua vim.diagnostic.reset()<CR>", desc = "Stop lsp servers" },
      { "<localleader>li", "<CMD>LspInfo<CR>",                                    desc = "LspInfo" },
    },
    ---@class PluginLspOpts
    opts = {
      -- Default callback
      on_attach = function(client, bufnr)
        lsp_theme()
        lsp_keymaps.on_attach(client, bufnr)
      end,
      capabilities = {

      },
      servers = {
        lua_ls = {
          settings = {
            Lua = {
              completion = {
                callSnippet = "Replace",
              },
              workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
              },
            }
          }
        },
        rust_analyzer = {},
        hls = {},
        texlab = {
          on_attach = function(client, bufnr)
            lsp_theme()
            vim.o.wrap = true -- Wrap lines

            -- Save and build
            vim.keymap.set("n", "<leader>b", "<CMD>write<CR><CMD>TexlabBuild<CR>",
              lsp_keymaps.bufopts(bufnr, "Build Latex"))

            -- Open pdf in zathura
            vim.keymap.set("n", "<localleader><enter>", function()
              local file = vim.fn.expand("%:r") .. ".pdf"
              vim.fn.execute("!zathura '" .. file .. "' &", true)
            end, lsp_keymaps.bufopts(bufnr, "Open pdf in Zathura"))

            lsp_keymaps.on_attach(client, bufnr)
          end,
          settings = {
            texlab = {
              rootDirectory = "./",
              auxDirectory = "./out/",
              build = {
                executable = "latexmk",
                args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "-auxdir=./out/", "%f" },
              },
            },
          },
        },
        ltex = {
          autostart = false,
          filetypes = { "rust", "tex", "markdown" },
          settings = {
            ltex = {
              enabled = { "rust", "tex", "markdown" },
              ["ltex-ls"] = {
                logLevel = "warning",
              },
              checkFrequency = "save",
              language = "en-US",
              additionalRules = {
                enablePickyRules = true,
              },
            },
          },
        },
        jsonls = {
          -- lazy-load schemastore when needed
          on_new_config = function(new_config)
            new_config.settings.json.schemas = new_config.settings.json.schemas or {}
            vim.list_extend(new_config.settings.json.schemas, require("schemastore").json.schemas())
          end,
          settings = {
            json = {
              format = {
                enable = true,
              },
              validate = { enable = true },
            },
          },
        },
        pyright = {},
        ruff_lsp = {},
      },
      -- Setup functions:
      --   Returns true if the server should not be setup with lspconfig.
      ---@type table<string, fun(opts: table):boolean?>
      setup = {
        rust_analyzer = function(_)
          -- Setup found at rust-tools plugin
          return true
        end,
        hls = function(_)
          -- Setup found at haskell-tools plugin
          return true
        end,
        ruff_lsp = function(_)
          require("lazyvim.util").on_attach(function(client, _)
            if client.name == "ruff_lsp" then
              -- Disable hover in favor of Pyright
              client.server_capabilities.hoverProvider = false
            end
          end)
        end,
      }
    },
    ---@param opts PluginLspOpts
    config = function(_, opts)
      lsp_theme()

      local servers = opts.servers
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        opts.capabilities or {}
      )

      local function setup_server(server)
        local server_opts = vim.tbl_deep_extend(
          "force",
          { capabilities = vim.deepcopy(capabilities) },
          { on_attach = opts.on_attach },
          servers[server] or {}
        )

        -- Run setup function
        if opts.setup[server] then
          if opts.setup[server](server_opts) then
            return
          end
        end

        require("lspconfig")[server].setup(server_opts)
      end

      for server in pairs(opts.servers) do
        setup_server(server)
      end

      -- Apply default handler for mason
      require("mason-lspconfig").setup_handlers({ setup_server })
    end
  },
}
