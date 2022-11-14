local keymap = vim.api.nvim_set_keymap

local opts = { silent = true, noremap = true }

vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- disable keys
keymap("n", "<Left>", "<Nop>", opts)
keymap("n", "<Right>", "<Nop>", opts)
keymap("n", "<Up>", "<Nop>", opts)
keymap("n", "<Down>", "<Nop>", opts)
keymap("n", "<C-z>", "<Nop>", opts)

-- Jump between windows
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- resize window
keymap("n", "<C-Up>", "<CMD>resize -5<CR>", opts)
keymap("n", "<C-Down>", "<CMD>resize +5<CR>", opts)
keymap("n", "<C-Left>", "<CMD>vertical resize -5<CR>", opts)
keymap("n", "<C-Right>", "<CMD>vertical resize +5<CR>", opts)

-- Move between tabs

-- Toggle code outline
keymap("n", "<localleader>o", ":AerialToggle<CR>", opts)

-- Clear highlights
keymap("n", "<leader>h", "<CMD>nohlsearch<CR>", opts)

-- Completion
keymap("n", "<leader>l", "<CMD>COQnow --shut-up<CR>", { noremap = true, silent = true, desc = "Start COQ" })

-- Neotree
keymap(
	"n",
	"<localleader>fd",
	"<CMD>Neotree filesystem<CR>",
	{ noremap = true, silent = true, desc = "Open filesystem" }
)
keymap(
	"n",
	"<localleader>gs",
	"<CMD>Neotree float git_status<CR>",
	{ noremap = true, silent = true, desc = "Show git status" }
)
vim.cmd([[nnoremap \ :Neotree reveal<cr>]]) -- Makes Neotree hijack netrw

-- Telescope
keymap("n", "<localleader>ff", ":Telescope find_files<CR>", opts)
keymap("n", "<localleader>ft", ":Telescope live_grep<CR>", opts)
keymap("n", "<localleader>fb", ":Telescope buffers<CR>", opts)

-- Move current line
keymap("i", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("i", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-------------------
-- Visual ---------
-------------------

keymap("x", "<A-j>", ":m '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":m '<-2<CR>gv-gv", opts)

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
