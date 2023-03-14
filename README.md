# dotfiles

My config files for my laptop and desktop running Fedora.

## Applications

| Name                 | Description                      |
| -------------------- | -------------------------------- |
| **[xmonad][xmonad]** | Window manager                   |
| **zsh**              | zsh shell with oh-my-zsh         |
| **alacritty**        | Terminal emulator                |
| **tmux**             | Terminal multiplexer             |
| **[Eww][eww]**       | Widgets and bar                  |
| **Rofi**             | Application and command launcher |
| **Dunst**            | Notifications                    |
| **[picom][picom]**   | Compositor picom fork            |

[xmonad]: ./.config/xmonad/
[eww]: https://github.com/elkowar/eww
[picom]: https://github.com/Arian8j2/picom-jonaburg-fix

## Themes

- [Nordic GTK](https://github.com/EliverLara/Nordic/)
- [Papirus Icon Theme](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme)

## Fonts

- [MesloLGMDZ Nerd Font (Dotted Zero)](https://github.com/ryanoasis/nerd-fonts/releases/)

## Setup

The instructions below is only intended to be used by me.
They may break something and I take no responsibility if that happens.

Start by running install-script:

```bash
wget https://raw.githubusercontent.com/sebostien/dotfiles/master/post_install.sh
chmod +x ./post_install.sh
./post_install.sh
```

Clone this repository

```bash
cd ~
echo 'alias config="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"' >> $HOME/.zshrc
source ~/.zshrc
git clone --bare https://www.github.com/sebostien/dotfiles.git $HOME/dotfiles
config checkout
config config --local status.showUntrackedFiles no
```

Done!
