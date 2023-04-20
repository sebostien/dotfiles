require("mason").setup({
  ui = {
    border = "rounded",
  },
})
require("mason-lspconfig").setup({
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
})
require("fidget").setup({}) -- Load status

local K = require("custom.lsp.lsp_keymap")

-- Mapping from Mason name => lspconfig name
-- https://github.com/williamboman/mason-lspconfig.nvim/blob/main/doc/server-mapping.md
require("mason-lspconfig").setup_handlers({

  -- Default handler
  function(server_name)
    require("lspconfig")[server_name].setup({
      on_attach = K.on_attach,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
    })
  end,

  ["eslint"] = function()
    require("lspconfig")["eslint"].setup({
      on_attach = K.on_attach,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      settings = {
        packageManager = "yarn",
      },
    })
  end,

  ["tsserver"] = function()
    require("lspconfig")["tsserver"].setup({
      on_attach = function(client, bufnr)
        client.server_capabilities.documentFormattingProvider = false
        K.on_attach(client, bufnr)
      end,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      settings = {},
    })
  end,

  ["lua_ls"] = function()
    require("neodev").setup()
    require("lspconfig")["lua_ls"].setup({
      on_attach = function(client, bufnr)
        client.server_capabilities.documentFormattingProvider = false
        K.on_attach(client, bufnr)
      end,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      -- Server-specific settings...
      settings = {
        Lua = {
          completion = {
            callSnippet = "Replace",
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
            checkThirdParty = false,
          },
        },
      },
    })
  end,

  ["rust_analyzer"] = function()
    local rt = require("rust-tools")
    rt.setup({
      tools = {
        executor = require("rust-tools.executors").termopen,
      },
      server = {
        on_attach = function(client, bufnr)
          K.on_attach(client, bufnr)
          -- Popup list from rt
          vim.keymap.set("n", "<Leader>ca", rt.code_action_group.code_action_group, K.bufopts(bufnr, "Code actions"))
        end,
      },
    })
  end,

  ["jsonls"] = function()
    require("lspconfig")["jsonls"].setup({
      on_attach = K.on_attach,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      -- Server-specific settings...
      settings = {
        json = {
          schemas = require("schemastore").json.schemas(),
          validate = { enable = true },
        },
      },
    })
  end,

  ["hls"] = function()
    local ht = require("haskell-tools")
    ht.setup({
      hls = {
        on_attach = function(client, bufnr)
          K.on_attach(client, bufnr)
        end,
      },
      tools = {
        hoogle = {
          mode = "browser",
          stylize_markdown = true,
          auto_focus = true,
        },
      },
    })
  end,

  ["texlab"] = function()
    require("lspconfig")["texlab"].setup({
      on_attach = function(client, bufnr)
        vim.o.wrap = true -- Wrap lines

        -- Save and build
        vim.keymap.set("n", "<leader>b", "<CMD>write<CR><CMD>TexlabBuild<CR>", K.bufopts(bufnr, "Build Latex"))

        -- Open pdf in zathura
        vim.keymap.set("n", "<localleader><enter>", function()
          local file = vim.fn.expand("%:r") .. ".pdf"
          vim.fn.execute("!zathura '" .. file .. "' &", true)
        end, K.bufopts(bufnr, "Open pdf in Zathura"))

        K.on_attach(client, bufnr)
      end,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
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
    })
  end,
  ["ltex"] = function() end, -- Do not remove or the server starts twice. Se below
})

require("lspconfig")["ltex"].setup({
  on_attach = K.on_attach,
  capabilities = vim.lsp.protocol.make_client_capabilities(),
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
        languageModel = "/home/sn/LanguageTool/",
        enablePickyRules = true,
        motherTongue = "en",
      },
    },
  },
})
