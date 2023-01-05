
require("texmagic").setup({
  engines = {
    pdflatex = {
      executable = "latexmk",
      args = {
        "-pdflatex",
        "-interaction=nonstopmode",
        "-synctex=1",
        "-outdir=.build",
        "-pv",
        "%f",
      },
      isContinuous = false,
    },
  },
})

vim.g["tex_flavor"] = "latex"
