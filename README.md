# dotfiles

Config files for XMonad running on Fedora.

## Applications

| Name                 | Description                                           |
| -------------------- | ----------------------------------------------------- |
| **[xmonad][xmonad]** | Window manager                                        |
| **Firefox**          | Web browser. Custom css with [Firefox Onebar][onebar] |
| **zsh**              | Shell with oh-my-zsh                                  |
| **wezterm**          | Terminal emulator                                     |
| **tmux**             | Terminal multiplexer                                  |
| **[Eww][eww]**       | Widgets and bar                                       |
| **Rofi**             | Application and command launcher                      |
| **Dunst**            | Notifications                                         |
| **[picom][picom]**   | Compositor, picom fork                                |

[xmonad]: https://github.com/sebostien/dotfiles/tree/master/.config/xmonad
[onebar]: https://codeberg.org/Freeplay/Firefox-Onebar
[eww]: https://github.com/elkowar/eww
[picom]: https://github.com/Arian8j2/picom-jonaburg-fix

## Themes

- **GTK Theme:** [Aritim-Dark](https://github.com/Mrcuve0/Aritim-Dark/releases/tag/0.7)
- **Icon Theme:** [papirus-icon-theme](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme)
- **Cursor Theme:** [phinger-cursors](https://github.com/phisch/phinger-cursors)
- All other applications (NeoVim, wezterm) uses a variation of Ayu-Dark with my custom colors. See (NeoVim config)[https://github.com/sebostien/dotfiles/blob/master/.config/nvim/lua/colors.lua].

## Fonts

- [MesloLGMDZ Nerd Font (Dotted Zero)](https://github.com/ryanoasis/nerd-fonts/releases/)

## CLI Applications

| Name       | Description           |
| ---------- | --------------------- |
| bat        | cat with colors       |
| eza        | ls alternative        |
| fclone     | Duplicate file finder |
| fd-find    | find alternative      |
| fselect    | Find with SQL Syntax  |
| fzf        | Fuzzy Finder          |
| hyperfine  | Benchmarking tool     |
| kalker     | Calculator            |
| ripgrep    | grep in files         |
| tealdeer   | Simple help-pages     |
| yazi       | TUI File manager      |

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
