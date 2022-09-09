#!/bin/bash
#
#  ________  ________      
# |\   ____\|\   ___  \      Sebastian Nielsen (sebostien)
# \ \  \___|\ \  \\ \  \     http://www.github.com/sebostien/
#  \ \_____  \ \  \\ \  \  
#   \|____|\  \ \  \\ \  \ 
#     ____\_\  \ \__\\ \__\
#    |\_________\|__| \|__|
#    \|_________|          
#    

if [ -f /etc/os-release ]; then
    . /etc/os-release
    if [ "$NAME" != "Ubuntu" ]; then
        echo "This script is intended for WSL Ubuntu"
        exit 1
    fi
fi

# Check if sudo
if [[ "$EUID" = 0 ]]; then
    echo "Don't run this script with sudo"
    exit 1
fi

mkdir ~/install_tmp
cd ~/install_tmp || exit 1

# Update
sudo apt update
sudo apt upgrade
sudo apt install software-properties-common

# Switch to zsh with oh-my-zsh
curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh | sh

# Install plugins for oh-my-zsh
git clone https://github.com/zsh-users/zsh-autosuggestions "$ZSH_CUSTOM/plugins/zsh-autosuggestions"
git clone https://github.com/supercrabtree/k "$ZSH_CUSTOM/plugins/k"

# Github CLI
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-key C99B11DEB97541F0
sudo apt-add-repository https://cli.github.com/packages
sudo apt update
sudo apt install gh

# Neofetch
sudo wget -O /usr/local/bin/neofetch https://raw.githubusercontent.com/dylanaraps/neofetch/master/neofetch
sudo chmod a+x /usr/local/bin/neofetch

# Some useful packages
sudo apt install bat httpie neovim tldr ripgrep fd-find

# Rustup, rustc, cargo
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# nvm, node, npm, yarn
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
. ~/.nvm/nvm.sh # Load nvm
nvm install node
curl -o- -L https://yarnpkg.com/install.sh | bash

# Change default shell
echo "Change shell to zsh:"
chsh -s "$(which zsh)"

# Remove temporary used by the script files
cd ~ || exit 1
rm -rf ~/install_tmp
