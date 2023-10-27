#!/bin/sh

fedora_version=$(awk '{print $3}' < /etc/fedora-release)

sudo rpm --import "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xD6BC243565B2087BC3F897C9277A7293F59E4889"
sudo curl -L -o /etc/yum.repos.d/miktex.repo "https://miktex.org/download/fedora/$fedora_version/miktex.repo"

sudo dnf upgrade

sudo dnf install fzf ripgrep

# TexLab LaTex
sudo dnf install latexmk miktex
sudo dnf install texlive texlive-amsmath texlive-glossaries texlive-todonotes \
                 texlive-tcolorbox texlive-upquote texlive-adjustbox texlive-titling \
                 texlive-ulem texlive-euler texlive-euler-math texlive-standalone \
                 texlive-newpx texlive-mathpartir texlive-stmaryrd texlive-varwidth

# Dictionary
sudo dnf install aspell aspell-en aspell-sv
mkdir -p ~/.config/nvim/dict/
aspell -d en dump master | aspell -l en expand > ~/.config/nvim/dict/en.dict
aspell -d sv dump master | aspell -l sv expand > ~/.config/nvim/dict/sv.dict
