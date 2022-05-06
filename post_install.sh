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

optimize_dnf() {

    printf %60s |tr " " "="
    echo "\n Optimizing dnf"

    defaultyes=`cat /etc/dnf/dnf.conf | grep defaultyes | awk -F '=' '{print $NF}'`
    if [ -z "$defaultyes" ]; then
        echo "defaultyes=True" | sudo tee -a /etc/dnf/dnf.conf > /dev/null
    elif [ "$defaultyes" != "True" ]; then
        echo "Can't set defaultyes in dnf.conf, set manually"
    fi

    max_parallel_downloads=`cat /etc/dnf/dnf.conf | grep max_parallel_downloads | awk -F '=' '{print $NF}'`
    if [ -z "$max_parallel_downloads" ]; then
        echo "max_parallel_downloads=10" | sudo tee -a /etc/dnf/dnf.conf > /dev/null
    else
        echo "Can't set max_parallel_downloads in dnf.conf, set manually"
    fi

    printf %60s |tr " " "="
    echo
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
mkdir -p ~/Apps
cd ~/install_tmp

# Optimize dnf config
optimize_dnf

####################################
next_part "Adding third party repositories"
####################################

# VS Code
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'

# RPM Fusion 
sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm -y
sudo dnf install https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm -y
sudo dnf install -y rpmfusion-free-release-tainted

# RPM Sphere, for trayer
sudo dnf install https://github.com/rpmsphere/noarch/raw/master/r/rpmsphere-release-34-2.noarch.rpm -y

# Add Fedora third party repository
sudo dnf install fedora-workstation-repositories

# Add core plugins
sudo dnf install -y dnf-plugins-core

####################################
next_part "System update"
####################################

sudo dnf upgrade --refresh -y
sudo dnf groupupdate core -y
sudo dnf check
sudo dnf makecache

# TODO check if UEFI
sudo fwupdmgr get-devices
sudo fwupdmgr refresh --force
sudo fwupdmgr get-updates
sudo fwupdmgr update -y

# Update multimedia
sudo dnf group upgrade -y --with-optional Multimedia
sudo dnf groupupdate -y sound-and-video

####################################
next_part "Installing packages"
####################################

# Switch to zsh with oh-my-zsh
sudo dnf install -y util-linux-user zsh
curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh | sh

# Install plugins for oh-my-zsh
git clone https://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions
git clone https://github.com/supercrabtree/k $ZSH_CUSTOM/plugins/k

# Github CLI
sudo dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
sudo dnf install gh -y

# Applets
sudo dnf install -y network-manager-applet blueman pulseaudio-utils

# Some packages i use
sudo dnf install neofetch flameshot fzf bat tldr \
                 httpie alacritty exa rofi nitrogen \
                 nautilus dunst neovim playerctl \
                 pulseaduio vlc ffmpeg qalculate-gtk -y

sudo dnf install steam -y
sudo dnf install trayer -y

dnf install ffmpeg-libs compat-ffmpeg28 -y

# Pipewire
udo dnf install -y pipewire-alsa pipewire-plugin-jack pipewire-pulseaudio qjackctl pipewire-plugin-jack

# Discord
sudo dnf install discord -y

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
sudo dnf install code -Y

####################################
if ask "Install Flatpaks??"; then
    
    sudo dnf install -y flatpak
    flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    
    # Spotify
    sudo flatpak install flathub com.spotify.Client


fi

####################################
next_part "Installing themes"
###################################

wget -P ./ https://github.com/EliverLara/Nordic/releases/download/v2.1.0/Nordic-darker-v40.tar.xz
tar xf ./Nordic-darker-v40.tar.xz
mv ./Nordic-darker-v40 ~/.themes/Nordic-darker-v40

# Dark mode for gnome
gsettings set org.gnome.desktop.interface gtk-theme Nordic-darker-v40
gsettings set org.gnome.desktop.wm.preferences theme Nordic-darker-v40

########################################
next_part "Installing optional programs"
########################################

####################################
if ask "Install Nord VPN client?"; then
    sudo dnf install -y https://repo.nordvpn.com/yum/nordvpn/centos/noarch/Packages/n/nordvpn-release-1.0.0-1.noarch.rpm
    sudo dnf install -y nordvpn
fi

####################################
if ask "Install picom dependencies?"; then
    # Picom dependencies
    sudo dnf install -y dbus-devel gcc git libconfig-devel libdrm-devel \
                        libev-devel libX11-devel libX11-xcb libXext-devel \
                        libxcb-devel mesa-libGL-devel meson pcre-devel \
                        pixman-devel uthash-devel xcb-util-image-devel \
                        xcb-util-renderutil-devel xorg-x11-proto-devel
fi

####################################
if ask "Install Eww?"; then
    
    cd ~/Apps

    # Eww Widgets dependencies
    sudo dnf install -y gtk3-devel pango-devel gdk-pixbuf2-devel \
                        cairo-devel cairo-gobject-devel glib2-devel

    git clone https://github.com/elkowar/eww
    cd eww

    cargo build --release
    chmod +x ./target/release/eww
    sudo ln -s ~/Apps/eww/target/release/eww /usr/bin/eww

    cd ~/install_tmp
fi

####################################
# Fonts
####################################

if ask "Install fonts?"; then
    next_part "Installing fonts"
    
    sudo dnf install unzip -y

    # Microsoft fonts
    sudo dnf install -y curl cabextract xorg-x11-font-utils fontconfig
    sudo rpm -i https://downloads.sourceforge.net/project/mscorefonts2/rpms/msttcore-fonts-installer-2.6-1.noarch.rpm
    
    # Roboto
    sudo dnf install google-roboto-fonts
    
    # Cascadia Code
    CASCADIA="https://github.com/microsoft/cascadia-code/releases/download/v2111.01/CascadiaCode-2111.01.zip"
    if [ "$(fc-list | grep -c 'Cascadia Code')" -lt 1 ]; then
        wget -P ./ $CASCADIA
        unzip ./CascadiaCode-2111.01.zip -d ./CascadiaCode-2111.01
        sudo mv ./CascadiaCode-2111.01/ttf/static/* /usr/share/fonts
    fi

    # Meslo Nerd Font
    MESLO="https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Meslo.zip"
    if [ "$(fc-list | grep -c 'MesloLGS')" -lt 1 ]; then
        wget -P ./ $MESLO
        unzip ./Meslo.zip -d ./Meslo
        sudo mv ./Meslo/*.ttf /usr/share/fonts
    fi
    
    fc-cache -f -v

fi

####################################

sudo dnf autoremove -y

# Change default shell
echo "Change shell to zsh:"
chsh -s "$(which zsh)"

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
echo "Picom Compositor --> https://github.com/yshui/picom"
echo
echo "Reboot when done"

