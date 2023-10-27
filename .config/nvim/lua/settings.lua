local M = {
  project_dirs = { "~/", "~/Documents/", "~/.config", "~/Desktop/" }
}

local o = vim.o

o.guifont = "Hasklug NF"
o.termguicolors = true -- Correct terminal colors

o.viminfo = "'1000"    -- Increase the size of file history
o.scrolloff = 8        -- Always keep space when scrolling to bottom/top edge
o.signcolumn = "yes"

o.swapfile = false
o.backup = false
o.undodir = os.getenv("HOME") .. "/.vim/undodir"
o.undofile = true   -- Sets undo to file

o.timeoutlen = 500  -- Faster completion
o.splitright = true -- Vertical splits will automatically be to the right
o.updatetime = 200

o.wildmode = "list:longest"
o.pumheight = 10 -- Max num of items in completion menu

o.incsearch = true
o.smartcase = true  -- Uses case in search
o.ignorecase = true -- Needed for smartcase

o.number = true
o.relativenumber = true

o.wrap = false

o.autoindent = true
o.expandtab = true   -- Use spaces instead of tabs
o.shiftwidth = 2     -- Change a number of space characeters inserted for indentation
o.smarttab = true    -- Makes tabbing smarter will realize you have 2 vs 4
o.smartindent = true -- Makes indenting "smarter"
o.softtabstop = 2    -- Insert 2 spaces for a tab
o.tabstop = 2        -- Insert 2 spaces for a tab
o.list = true

-- Remove mouse pop-up text
vim.cmd([[ aunmenu PopUp.How-to\ disable\ mouse ]])
vim.cmd([[ aunmenu PopUp.-1- ]])

vim.cmd([[ nnoremap \ :Neotree reveal<cr> ]]) -- Makes Neotree hijack netrw

-- Windows to close with "q"
vim.cmd([[ autocmd FileType help,qf,lspinfo nnoremap <buffer><silent> q :close<CR> ]])
vim.cmd([[ autocmd FileType man nnoremap <buffer><silent> q :quit<CR> ]])

return M
