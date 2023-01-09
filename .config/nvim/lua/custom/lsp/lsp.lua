require("mason").setup({
  ui = {
    border = "rounded",
  },
})
require("mason-lspconfig").setup({
  ensure_installed = {
    "sumneko_lua",
    "tsserver",
    "jsonls",
    "rust_analyzer",
    "eslint",
    "marksman",
    "texlab",
    "ruff_lsp",
  },
})

require("lspconfig.ui.windows").default_options.border = "rounded"

local border = {
  { "╭", "FloatBorder" },
  { "─", "FloatBorder" },
  { "╮", "FloatBorder" },
  { "│", "FloatBorder" },
  { "╯", "FloatBorder" },
  { "─", "FloatBorder" },
  { "╰", "FloatBorder" },
  { "│", "FloatBorder" },
}

-- Fix border globally
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or border
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

local function bufopts(bufnr, desc)
  return {
    noremap = true,
    silent = true,
    buffer = bufnr,
    desc = desc,
  }
end

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { noremap = true, silent = true, desc = "Go to prev diagnostics" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { noremap = true, silent = true, desc = "Go to mext diagnostics" })
vim.keymap.set(
  "n",
  "<space>q",
  "<CMD>Trouble document_diagnostics<CR>",
  { noremap = true, silent = true, desc = "Show all diagnostics" }
)
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, bufopts(bufnr, "Format file"))

-- https://github.com/neovim/nvim-lspconfig
local on_attach = function(_, bufnr)
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts(bufnr, "Go to definition"))
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts(bufnr, "Go to declaration"))
  vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, bufopts(bufnr, "Go to type definition"))
  vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts(bufnr, "Go to references"))
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts(bufnr, "Go to implementations"))

  vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts(bufnr, "Hover"))
  vim.keymap.set("n", "<leader>k", vim.lsp.buf.signature_help, bufopts(bufnr, "Signature help"))
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts(bufnr, "Rename"))
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, bufopts(bufnr, "Code actions"))
end

-- Mapping from Mason name => lspconfig name
-- https://github.com/williamboman/mason-lspconfig.nvim/blob/main/doc/server-mapping.md
require("mason-lspconfig").setup_handlers({
  -- Default handler
  function(server_name)
    require("lspconfig")[server_name].setup({
      on_attach = on_attach,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
    })
  end,

  ["eslint"] = function()
    require("lspconfig")["eslint"].setup({
      on_attach = on_attach,
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
        on_attach(client, bufnr)
      end,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      settings = {},
    })
  end,

  ["sumneko_lua"] = function()
    require("neodev").setup()
    require("lspconfig")["sumneko_lua"].setup({
      on_attach = function(client, bufnr)
        client.server_capabilities.documentFormattingProvider = false
        on_attach(client, bufnr)
      end,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      -- Server-specific settings...
      settings = {
        Lua = {
          completion = {
            callSnippet = "Replace",
          },
        },
      },
    })
  end,

  ["rust_analyzer"] = function()
    local rt = require("rust-tools")
    rt.setup({
      server = {
        on_attach = function(client, bufnr)
          on_attach(client, bufnr)
          -- Popup list from rt
          vim.keymap.set("n", "<Leader>ca", rt.code_action_group.code_action_group, bufopts(bufnr, "Code actions"))
        end,
      },
    })
  end,

  ["jsonls"] = function()
    require("lspconfig")["jsonls"].setup({
      on_attach = on_attach,
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
          on_attach(client, bufnr)
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
        vim.keymap.set("n", "<leader>b", "<CMD>write<CR><CMD>TexlabBuild<CR>", bufopts(bufnr, "Build Latex"))

        -- Open pdf in zathura
        vim.keymap.set("n", "<localleader><enter>", function()
          local file = vim.fn.expand("%:r") .. ".pdf"
          vim.fn.execute("!zathura '" .. file .. "' &", true)
        end, bufopts(bufnr, "Open pdf in Zathura"))

        on_attach(client, bufnr)
      end,
      capabilities = vim.lsp.protocol.make_client_capabilities(),
      settings = {},
    })
  end,
})
