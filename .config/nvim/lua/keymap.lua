local keymap = vim.keymap

-- disable keys
keymap.set("n", "<Left>", "<Nop>")
keymap.set("n", "<Right>", "<Nop>")
keymap.set("n", "<Up>", "<Nop>")
keymap.set("n", "<Down>", "<Nop>")
keymap.set("n", "<C-z>", "<Nop>")

-- Jump between windows
keymap.set("n", "<C-k>", "<CMD>wincmd k<CR>")
keymap.set("n", "<C-j>", "<CMD>wincmd j<CR>")
keymap.set("n", "<C-h>", "<CMD>wincmd h<CR>")
keymap.set("n", "<C-l>", "<CMD>wincmd l<CR>")

-- resize window
keymap.set("n", "<C-Left>", "<CMD>vertical resize -5<CR>")
keymap.set("n", "<C-Up>", "<CMD>resize -5<CR>")
keymap.set("n", "<C-Right>", "<CMD>vertical resize +5<CR>")
keymap.set("n", "<C-Down>", "<CMD>resize +5<CR>")

-- split window
keymap.set("n", "<C-A-k>", "<C-w>t<C-w>K")
keymap.set("n", "<C-A-h>", "<C-w>t<C-w>H")

