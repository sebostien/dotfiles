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

-- Clear highlights
keymap("n", "<leader>h", "<CMD>nohlsearch<CR>", opts)

-- Marks
keymap("n", "<localleader>ma", "<CMD>lua require('harpoon.mark').add_file() <CR>", opts)
keymap("n", "<localleader>ms", "<CMD>lua require('harpoon.ui').toggle_quick_menu() <CR>", opts)
keymap("n", "<localleader>mf", "<CMD>Telescope harpoon marks<CR>", opts)

-- Undotree
keymap("n", "<localleader>u", "<CMD>UndotreeToggle<CR>", opts)

-- Color highlight
keymap("n", "<localleader>c", ":CccHighlighterToggle <CR>", opts)

-- Sessions
keymap("n", "<leader>ss", "<CMD>lua require('resession').save() <CR>", opts)
keymap("n", "<leader>sl", "<CMD>lua require('resession').load()<CR>", opts)
keymap("n", "<leader>sd", "<CMD>lua require('resession').delete()<CR>", opts)

-- Overseer
keymap("n", "<localleader>tt", "<CMD>OverseerToggle<CR>", opts)
keymap("n", "<localleader>tr", "<CMD>OverseerRun<CR>", opts)

-- Neotree
keymap(
  "n",
  "<localleader>fd",
  "<CMD>Neotree toggle filesystem position=left <CR>",
  { noremap = true, silent = true, desc = "Open filesystem" }
)

-- Git
keymap(
  "n",
  "<localleader>gs",
  "<CMD>Git<CR>",
  { noremap = true, silent = true, desc = "Fugitive git status" }
)

-- Telescope
keymap("n", "<localleader>ff", ":Telescope find_files<CR>", opts)
keymap("n", "<localleader>fg", ":Telescope git_files<CR>", opts)
keymap("n", "<localleader>ft", ":Telescope live_grep<CR>", opts)
keymap("n", "<localleader>fb", ":Telescope buffers<CR>", opts)

-- Move current line
keymap("i", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("i", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-------------------
-- Visual ---------
-------------------

-- Move lines
keymap("x", "<A-j>", ":m '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":m '<-2<CR>gv-gv", opts)

-- Stay in indent mode when indenting
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Clipboard
keymap("v", "<leader>y", '"+y', opts)
keymap("x", "<leader>y", '"+y', opts)
keymap("n", "<leader>p", '"+p', opts)
