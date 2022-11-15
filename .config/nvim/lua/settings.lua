local o = vim.o

o.guifont = "Hasklug NF"
o.ignorecase = true
o.list = true
o.blend = 10
o.smartcase = true

-- o.showmode = false

o.expandtab = true
o.smartindent = true
o.tabstop = 2
o.shiftwidth = 2

vim.cmd([[set number relativenumber]])
vim.cmd([[nnoremap \ :Neotree reveal<cr>]]) -- Makes Neotree hijack netrw

-- Windows to close with "q"
vim.cmd([[autocmd FileType help,qf,lspinfo nnoremap <buffer><silent> q :close<CR>]])
vim.cmd([[autocmd FileType man nnoremap <buffer><silent> q :quit<CR>]])
