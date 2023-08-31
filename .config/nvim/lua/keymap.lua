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
-- keymap("n", "<localleader>ln", "<CMD>NullLsInfo<CR>", opt_sn)

-- Current buffer toggles
keymap("n", "<leader>tc", "<CMD>CccHighlighterToggle <CR>", opt_sn)
keymap("n", "<leader>ti", "<CMD>IlluminateToggle<CR>", opt_sn)
-- keymap("n", "<leader>tl", require("lsp_lines").toggle, opt_sn_desc("Toggle lsp lines"))

-- UndoTree
keymap("n", "<localleader>u", "<CMD>UndotreeToggle<CR>", opt_sn)

-- Lazy
keymap("n", "<localleader>ll", "<CMD>Lazy<CR>", opt_sn)

-- Files and folders
keymap("n", "<localleader>fcd", util.telescope_cd, opt_sn_desc("Change directory"))

-- Open files
keymap("n", "<localleader>op", function()
  util.telescope_files({ "%.pdf" }, "zathura")
end, opt_sn_desc("Open file in zathura"))

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

-- Stay in visual mode when indenting
keymap("v", "<", "<gv", opt_sn)
keymap("v", ">", ">gv", opt_sn)

-- Yank/put system clipboard
keymap("v", "<leader>y", '"+y', opt_sn)
keymap("n", "<leader>y", '"+y', opt_sn)
keymap("n", "<leader>Y", '"+Y', opt_sn) -- Yank whole line
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
