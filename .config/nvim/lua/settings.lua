local o = vim.o

o.blend = 10
o.ignorecase = true -- Needed for smartcase
o.smartcase = true -- Uses case in search
o.guifont = "Hasklug NF"
o.list = true
o.incsearch = true
o.viminfo = "'1000" -- Increase the size of file history
o.wrap = false -- Display long lines as just one line
o.scrolloff = 8 -- Always keep space when scrolling to bottom/top edge
o.pumheight = 10 -- Max num of items in completion menu
o.undofile = true -- Sets undo to file
o.termguicolors = true -- Correct terminal colors
o.timeoutlen = 500 -- Faster completion
o.splitright = true -- Vertical splits will automatically be to the right
o.updatetime = 1000
o.wildmode = "list:longest"
-- o.showmode = false

o.autoindent = true
o.expandtab = true -- Use spaces instead of tabs
o.shiftwidth = 2 -- Change a number of space characeters inserted for indentation
o.smarttab = true -- Makes tabbing smarter will realize you have 2 vs 4
o.smartindent = true -- Makes indenting "smarter"
o.softtabstop = 2 -- Insert 2 spaces for a tab
o.tabstop = 2 -- Insert 2 spaces for a tab

vim.cmd([[set number relativenumber]]) -- Relative linenumbers
vim.cmd([[nnoremap \ :Neotree reveal<cr>]]) -- Makes Neotree hijack netrw

-- Windows to close with "q"
vim.cmd([[autocmd FileType help,qf,lspinfo nnoremap <buffer><silent> q :close<CR>]])
vim.cmd([[autocmd FileType man nnoremap <buffer><silent> q :quit<CR>]])

