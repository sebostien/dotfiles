local util = require("util")
local keymap = vim.keymap.set

local opt_sn = { silent = true, noremap = true }

local function opt_sn_desc(desc)
  return { silent = true, noremap = true, desc = desc }
end

vim.g.mapleader = " " -- Commands involving current buffer
vim.g.maplocalleader = "," -- Commands beyond buffer

-- Jump between windows
keymap("n", "<C-k>", "<C-w>k", opt_sn)
keymap("n", "<C-j>", "<C-w>j", opt_sn)
keymap("n", "<C-h>", "<C-w>h", opt_sn)
keymap("n", "<C-l>", "<C-w>l", opt_sn)

-- resize window
keymap("n", "<C-Up>", "<CMD>resize -5<CR>", opt_sn)
keymap("n", "<C-Down>", "<CMD>resize +5<CR>", opt_sn)
keymap("n", "<C-Left>", "<CMD>vertical resize -5<CR>", opt_sn)
keymap("n", "<C-Right>", "<CMD>vertical resize +5<CR>", opt_sn)

-- Clear highlights
keymap("n", "<leader>h", "<CMD>nohlsearch<CR>", opt_sn)

-- Lsp Stuff
keymap("n", "<localleader>lr", "<CMD>LspRestart<CR><CMD>e<CR>", opt_sn_desc("Restart lsp servers"))
keymap("n", "<localleader>ls", "<CMD>LspStop<CR><CMD>lua vim.diagnostic.reset()<CR>", opt_sn_desc("Stop lsp servers"))
keymap("n", "<localleader>ll", "<CMD>LspInfo<CR>", opt_sn)
keymap("n", "<localleader>lm", "<CMD>Mason<CR>", opt_sn)
keymap("n", "<localleader>ln", "<CMD>NullLsInfo<CR>", opt_sn)

-- Marks
keymap("n", "<localleader>ma", "<CMD>lua require('harpoon.mark').add_file() <CR>", opt_sn)
keymap("n", "<localleader>ms", "<CMD>lua require('harpoon.ui').toggle_quick_menu() <CR>", opt_sn)
keymap("n", "<localleader>mf", "<CMD>Telescope harpoon marks<CR>", opt_sn)

-- Current buffer toggles
keymap("n", "<leader>tc", "<CMD>CccHighlighterToggle <CR>", opt_sn)
keymap("n", "<leader>ti", "<CMD>IlluminateToggle<CR>", opt_sn)
keymap("n", "<leader>tl", require("lsp_lines").toggle, opt_sn_desc("Toggle lsp lines"))

-- UndoTree
keymap("n", "<localleader>u", "<CMD>UndotreeToggle<CR>", opt_sn)

-- Packer
keymap("n", "<localleader>ps", "<CMD>PackerSync<CR>", opt_sn)
keymap("n", "<localleader>pc", "<CMD>PackerCompile<CR>", opt_sn)

-- Git
keymap("n", "<localleader>gs", "<CMD>Git<CR>", opt_sn_desc("Fugitive git status"))

-- Files and folders
keymap("n", "<localleader>fd", "<CMD>Neotree toggle filesystem position=left <CR>", opt_sn_desc("Open filesystem"))
keymap("n", "<localleader>ff", "<CMD>Telescope find_files<CR>", opt_sn)
keymap("n", "<localleader>fg", "<CMD>Telescope git_files<CR>", opt_sn)
keymap("n", "<localleader>ft", "<CMD>Telescope live_grep<CR>", opt_sn)
keymap("n", "<localleader>fb", "<CMD>Telescope buffers<CR>", opt_sn)
keymap("n", "<localleader>fh", "<CMD>Telescope command_history<CR>", opt_sn)
keymap("n", "<localleader>fcd", util.telescope_cd, opt_sn_desc("Change directory"))

-- Open files
keymap("n", "<localleader>op", function()
  util.telescope_files({ "%.pdf" }, "zathura")
end, opt_sn_desc("Open file in zathura"))

-- Help
keymap("n", "<localleader>hh", "<CMD>Telescope help_tags<CR>", opt_sn)
keymap("n", "<localleader>hm", "<CMD>Telescope man_pages<CR>", opt_sn)
keymap("n", "<localleader>hk", "<CMD>Telescope keymaps<CR>", opt_sn)

-- Move current line
keymap("i", "<A-j>", "<Esc>:m .+1<CR>==gi", opt_sn)
keymap("i", "<A-k>", "<Esc>:m .-2<CR>==gi", opt_sn)

-- Search replace word under cursor
keymap(
  "n",
  "<leader>s",
  [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
  opt_sn_desc("Substitue word under cursor")
)

-- Move lines
keymap("x", "<A-j>", ":m '>+1<CR>gv-gv", opt_sn)
keymap("x", "<A-k>", ":m '<-2<CR>gv-gv", opt_sn)

-- Stay in indent mode when indenting
keymap("v", "<", "<gv", opt_sn)
keymap("v", ">", ">gv", opt_sn)

-- Clipboard
keymap("v", "<leader>y", '"+y', opt_sn)
keymap("n", "<leader>y", '"+y', opt_sn)
keymap("n", "<leader>Y", '"+Y', opt_sn) -- Yank to end of line
keymap("n", "<leader>p", '"+p', opt_sn)

-- Delete to void
keymap("n", "<leader>d", '"_d', opt_sn)
keymap("v", "<leader>d", '"_d', opt_sn)

-- Keep cursor in middle when paging
keymap("n", "<C-d>", "<C-d>zz", opt_sn)
keymap("n", "<C-u>", "<C-u>zz", opt_sn)

-- Keep cursor in middle when searching
keymap("n", "n", "nzz", opt_sn)
keymap("n", "N", "Nzz", opt_sn)

keymap("n", "Q", "<nop>", opt_sn)
