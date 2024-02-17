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

# Check if sudo
if [[ "$EUID" = 0 ]]; then
    echo "Don't run with sudo"
    exit 1
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
        Y* | y*) return 0 ;;
        *) return 1 ;;
    esac
}

optimize_dnf() {

    printf "%0.s=" {1..60}
    printf "\n Optimizing dnf\n\n"

    defaultyes=$(grep defaultyes </etc/dnf/dnf.conf | awk -F '=' '{print $NF}')
    if [ -z "$defaultyes" ]; then
        echo "defaultyes=True" | sudo tee -a /etc/dnf/dnf.conf >/dev/null
    elif [ "$defaultyes" != "True" ]; then
        echo "Can't set defaultyes to True in dnf.conf, set manually"
    fi

    max_parallel_downloads=$(grep max_parallel_downloads </etc/dnf/dnf.conf | awk -F '=' '{print $NF}')
    if [ -z "$max_parallel_downloads" ]; then
        echo "max_parallel_downloads=10" | sudo tee -a /etc/dnf/dnf.conf >/dev/null
    elif [ "$max_parallel_downloads" != "10" ]; then
        echo "Can't set max_parallel_downloads in dnf.conf, set manually"
    fi

    printf "%0.s=" {1..60}
    echo
}

# Prints string so its visible clearly
next_part() {
    output="------ $1 ------"
    len=${#output}
    printf %"$len"s " " | tr " " "="
    echo
    echo "$output"
    printf %"$len"s " " | tr " " "="
    echo
    echo
}

mkdir ~/install_tmp
mkdir -p ~/Apps
cd ~/install_tmp || exit 1

# Optimize dnf config
optimize_dnf

####################################
next_part "Adding third party repositories"
####################################

sudo dnf install 'dnf-command(config-manager)'

# RPM Fusion
sudo dnf install "https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm" -y
sudo dnf install "https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm" -y
sudo dnf install -y rpmfusion-free-release-tainted

# RPM Sphere, for trayer
sudo dnf install "https://github.com/rpmsphere/noarch/raw/master/r/rpmsphere-release-$(rpm -E %fedora)-1.noarch.rpm" -y

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

sudo fwupdmgr get-devices
sudo fwupdmgr refresh --force
sudo fwupdmgr get-updates
sudo fwupdmgr update -y

# Multimedia
sudo dnf group install -y -q Multimedia
sudo dnf group install -y -q sound-and-video

####################################
next_part "Installing packages"
####################################

# Switch to zsh with oh-my-zsh
sudo dnf install -y -q util-linux-user zsh
curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh | sh

# Install plugins for oh-my-zsh
git clone https://github.com/zsh-users/zsh-autosuggestions "$ZSH_CUSTOM/plugins/zsh-autosuggestions"

# Github CLI
sudo dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
sudo dnf install gh -y -q

# Applets
sudo dnf install -y network-manager-applet blueman

# Some packages I use, some more installed with cargo below
sudo dnf install -y -q \
  neofetch    \
  flameshot   \
  fzf         \
  bat         \
  tealdeer    \
  httpie      \
  rofi        \
  nitrogen    \
  nautilus    \
  dunst       \
  neovim      \
  playerctl   \
  vlc         \
  btop        \
  ripgrep     \
  fd-find     \
  stalonetray \
  aria2       \
  hyperfine   \
  docker      \
  docker-compose


# ffmpeg stuff
dnf install ffmpeg ffmpeg-libs compat-ffmpeg28 -y -q

# Pipewire
sudo dnf install -y -q pipewire-alsa pipewire-plugin-jack pipewire-pulseaudio qjackctl pipewire-plugin-jack

# Pip
sudo dnf install python3-pip -y -q

####################################
next_part "Installing tmux"
####################################
sudo dnf install tmux python3-tmuxp
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

####################################
if ask "Install Rust toolchain and crates.io binaries?"; then
    # Rustup, rustc, cargo
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

    cargo install -q fclones    # Duplicate file finder
    cargo install -q fselect    # Find with SQL syntax
    cargo install -q kalker     # CLI Calculator
    cargo install -q flamegraph # Flamegraph generator
    cargo install -q eza        # ls alternative
    cargo install -q zoxide     # cd alternative

    sudo dnf install libsmbclient-devel -y -q
    cargo install -q termscp    # Remote file transfer (SFTP...)

    cargo install --git https://github.com/typst/typst # Typesetting system
fi

if ask "Install Node toolchain?"; then
    # nvm, node, npm, yarn
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
    . ~/.nvm/nvm.sh # Load nvm
    nvm install node
    curl -o- -L https://yarnpkg.com/install.sh | bash
fi

####################################

SPOTIFY=0
DISCORD=0

if ask "Install Discord via Flatpak?"; then
    DISCORD=1
fi
if ask "Install Spotify via Flatpak?"; then
    SPOTIFY=1
fi

if [ $((SPOTIFY + ELEMENT + DISCORD)) -ge 1 ]; then
    sudo dnf install -y flatpak
    flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

    if [ $DISCORD -eq 1 ]; then 
        flatpak install app/com.discordapp.Discord/x86_64/stable -y
    fi
    if [ $SPOTIFY -eq 1 ]; then 
        flatpak install com.s potify.Client/x86_64/stable -y
    fi

fi

####################################
next_part "Installing themes"
###################################

wget -P ./ https://github.com/Mrcuve0/Aritim-Dark/archive/refs/tags/0.7.zip
unzip 0.7.zip -qq
mv ./Aritim-Dark/GTK/ ~/.themes/Aritim-Dark-GTK/

# Icons
sudo dnf install -y papirus-icon-theme

# Cursor
wget -cO- https://github.com/phisch/phinger-cursors/releases/latest/download/phinger-cursors-variants.tar.bz2 | tar xfj - -C ~/.icons

########################################
next_part "Installing optional programs"
########################################

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
EWW=0
YAZI=0

if ask "Install yazi"; then 
  YAZI=1;
fi

if ask "Install Eww?"; then
   EWW=1;
fi

if [ $YAZI -eq 1 ]; then
    cd ~/Apps || exit 1
    
    git clone https://github.com/sxyazi/yazi.git
    cd ~/Apps/yazi/ || exit 1
    cargo build --release
    sudo ln -s /home/sn/Apps/yazi/target/release/yazi /usr/bin/yazi

    cd ~/install_tmp || exit 1
fi

if [ $EWW -eq 1 ]; then
    cd ~/Apps || exit 1

    # Eww Widgets dependencies
    sudo dnf install -y gtk3-devel pango-devel gdk-pixbuf2-devel \
        cairo-devel cairo-gobject-devel glib2-devel gtk-layer-shell-devel

    git clone https://github.com/elkowar/eww
    cd ~/Apps/eww/ || exit 1
    cargo build --release
    sudo ln -s /home/sn/Apps/eww/target/release/eww /usr/bin/eww

    cd ~/install_tmp || exit 1
fi

####################################
# Fonts
####################################

if ask "Install fonts?"; then
    next_part "Installing fonts"

    sudo dnf install unzip -y

    # Anonymous Pro
    # https://www.marksimonson.com/fonts/view/anonymous-pro
    sudo dnf install msimonson-anonymouspro-fonts -y

    sudo dnf install rsms-inter-fonts -y

    # Meslo Nerd Font
    MESLO="https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.2/Meslo.zip"
    if [ "$(fc-list | grep -c 'Meslo')" -lt 1 ]; then
        wget -P ./ $MESLO
        unzip ./Meslo.zip -d ./Meslo
        sudo mv ./Meslo/*.ttf /usr/share/fonts
    fi

    # Hasklig Hasklug Nerd Font
    HASKLIG="https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.2/Hasklig.zip"
    if [ "$(fc-list | grep -c 'Hasklug')" -lt 1 ]; then
        wget -P ./ $HASKLIG
        unzip ./Hasklig.zip -d ./Hasklig
        sudo mv ./Hasklig/*.otf /usr/share/fonts
    fi

    # Fira Code Nerd Font
    FIRACODE="https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.2/FiraCode.zip"
    if [ "$(fc-list | grep -c 'Fira')" -lt 1 ]; then
        wget -P ./ $FIRACODE
        unzip ./FiraCode.zip -d ./FiraCode
        sudo mv ./FiraCode/*.ttf /usr/share/fonts
    fi

    fc-cache -f -v

fi

########################################
next_part "Setting default applications"
########################################

xdg-mime default org.pwmt.zathura.desktop application/pdf

####################################

sudo dnf autoremove -y

# Change default shell
echo "Change shell to zsh:"
chsh -s "$(which zsh)"

# Remove temporary files used by the script
cd ~ || exit 1
rm -rf ~/install_tmp

next_part "Done"

echo
echo "Auth GitHub cli:"
echo "    gh auth login"
echo
echo
echo "Finish setup of apps in ~/Apps/"
echo
echo "Please install these packages manually:"
echo
echo "    Picom Compositor --> https://github.com/Arian8j2/picom-jonaburg-fix"
echo "    Wez's term       --> https://wezfurlong.org/wezterm/install/linux.html#installing-on-fedora-and-rpm-based-systems"
echo
echo "Setup your window manager"
echo
echo "Reboot when done"
