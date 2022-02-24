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
    if [ "$NAME" != "Fedora Linux" ]; then
        echo "Running $NAME, this script only supports Fedora Linux"
        exit 1
    fi
fi

ask() {
    local reply
    echo -n "$1 [Y/n] "
    read -r reply </dev/tty

    # No answer => set reply to Y
    if [[ -z $reply ]]; then
        reply="Y"
    fi

    case "$reply" in
        Y*|y*) return 0 ;;
        *) return 1 ;;
    esac
}

# Prints string so its visible clearly
next_part() {
    output="------ $1 ------"
    len=${#output}
    printf %"$len"s |tr " " "="
    echo
    echo "$output"
    printf %"$len"s |tr " " "="
    echo
}

# Check if sudo
if [[ "$EUID" = 0 ]]; then
    echo "Don't run with sudo"
    exit 1
fi

mkdir ~/install_tmp
cd ~/install_tmp

####################################
next_part "System update"
####################################

# Add Fedora third party repository
sudo dnf install fedora-workstation-repositories

# Optimize dnf config
sudo echo -e "\nfastestmirror=True \nmax_parallel_downloads=10 \ndefaultyes=True " >> /etc/dnf/dnf.conf
sudo dnf upgrade --refresh -y
sudo dnf check
sudo dnf makecache

# TODO check if UEFI
#sudo fwupdmgr get-devices
#sudo fwupdmgr refresh --force
#sudo fwupdmgr get-updates
#sudo fwupdmgr update -y

####################################
next_part "Installing packages"
####################################

# Switch to zsh with oh-my-zsh
sudo dnf install -y util-linux-user zsh
curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh | sh

# Github CLI
sudo dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
sudo dnf install gh -y

# Applets
sudo dnf install -y network-manager-applet blueman

# Some packages i use
sudo dnf install -y neofetch flameshot fzf bat tldr \
                    httpie alacritty exa rofi nitrogen \
                    nautilus dunst trayer

# Google Chrome
sudo dnf config-manager --set-enabled google-chrome
sudo dnf install google-chrome-stable -y

# Pip
sudo dnf install python3-pip -y

# onefetch: git info tool
sudo dnf copr enable varlad/onefetch -y
sudo dnf install onefetch -y

# Rustup, rustc, cargo
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# nvm, node, npm, yarn
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
. ~/.nvm/nvm.sh # Load nvm
nvm install node
curl -o- -L https://yarnpkg.com/install.sh | bash

# VS Code
wget -P ./vscode.rpm https://code.visualstudio.com/sha/download?build=stable&os=linux-rpm-x64
sudo dnf install ./vscode.rpm

# Picom dependencies
sudo dnf install -y dbus-devel gcc git libconfig-devel libdrm-devel \
                    libev-devel libX11-devel libX11-xcb libXext-devel \
                    libxcb-devel mesa-libGL-devel meson pcre-devel \
                    pixman-devel uthash-devel xcb-util-image-devel \
                    xcb-util-renderutil-devel xorg-x11-proto-devel
        
# Eww Widgets dependencies
sudo dnf install -y gtk3-devel pango-devel gdk-pixbuf2-devel \
                    cairo-devel cairo-gobject-devel glib2-devel

####################################
if ask "Install fonts?"; then
    next_part "Installing fonts"
    
    sudo dnf install unzip -y

    CASCADIA="https://github.com/microsoft/cascadia-code/releases/download/v2111.01/CascadiaCode-2111.01.zip"
    if [ "$(fc-list | grep -c 'Cascadia Code')" -lt 1 ]; then
        wget -P ./ $CASCADIA
        unzip ./CascadiaCode-2111.01.zip -d ./CascadiaCode-2111.01
        mv ./CascadiaCode-2111.01/ttf/static/* /usr/share/fonts
    fi

    MESLO="https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Meslo.zip"
    if [ "$(fc-list | grep -c 'MesloLGS')" -lt 1 ]; then
        wget -P ./ $MESLO
        unzip ./Meslo.zip -d ./Meslo
        mv ./Meslo/*.ttf /usr/share/fonts
    fi
    
    fc-cache -f -v

fi
####################################

sudo dnf autoremove -y

# Change default shell
sudo chsh -s /usr/bin/zsh

# Remove temporary used by the script files
cd ~/
rm -rf ~/install_tmp

next_part "Done"

echo 
echo "Auth GitHub cli:"
echo "    gh auth login"
echo
echo "Setup your window manager"
echo
echo "Please install these packages manually:"
echo 
echo "Eww Widgets      --> https://elkowar.github.io/eww/eww.html"
echo "Picom Compositor --> https://github.com/yshui/picom"
echo
echo "Reboot when done"