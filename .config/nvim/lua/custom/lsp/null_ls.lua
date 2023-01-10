-- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
local null_ls = require("null-ls")
local helpers = require("null-ls.helpers")

local K = require("custom.lsp.lsp_keymap")

null_ls.setup({
  border = "rounded",
  sources = {
    -- Formatting
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.formatting.prettier.with({ extra_args = {} }),
    null_ls.builtins.formatting.rustfmt,
    null_ls.builtins.formatting.ruff,

    -- Code actions
    null_ls.builtins.code_actions.shellcheck,
    null_ls.builtins.code_actions.cspell, -- Spellchecker, https://github.com/streetsidesoftware/cspell
    null_ls.builtins.code_actions.proselint, -- Writing helper, https://github.com/amperser/proselint

    -- Diagnostics
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.diagnostics.cspell, -- Spellchecker, https://github.com/streetsidesoftware/cspell
    null_ls.builtins.diagnostics.proselint, -- Writing helper, https://github.com/amperser/proselint
  },
  on_attach = K.on_attach, -- Setup keymaps for client
})

local markdownlint = {
  method = null_ls.methods.DIAGNOSTICS,
  filetypes = { "markdown" },
  generator = null_ls.generator({
    command = "markdownlint",
    args = { "--stdin" },
    to_stdin = true,
    from_stderr = true,
    format = "line",
    check_exit_code = function(code, stderr)
      if code > 1 then
        print(stderr)
        return false
      end
    end,
    -- use helpers to parse the output from string matchers,
    -- or parse it manually with a function
    on_output = helpers.diagnostics.from_patterns({
      {
        pattern = [[:(%d+):(%d+) [%w-/]+ (.*)]],
        groups = { "row", "col", "message" },
      },
      {
        pattern = [[:(%d+) [%w-/]+ (.*)]],
        groups = { "row", "message" },
      },
    }),
  }),
}

null_ls.register(markdownlint)

-- Spelling tools

null_ls.disable("cspell")
vim.keymap.set("n", "<localleader>sc", function()
  require("null-ls").toggle("cspell")
end, { desc = "Toggle cspell" })

null_ls.disable("proselint")
vim.keymap.set("n", "<localleader>sp", function()
  require("null-ls").toggle("proselint")
end, { desc = "Toggle proselint" })
