# Dotfiles

My custom config files for Fedora with XMonad WM.

## Applications

Some applications I use

| Name                       | Description                                       |
| -------------------------- | ------------------------------------------------- |
| **[xmonad][xmonad]**       | Window manager                                    |
| **zsh**                    | zsh shell with oh-my-zsh                          |
| **alacritty**              | Terminal emulator                                 |
| **nautilus**               | File manager                                      |
| **Spotify**                | Music, spicetify [Dribbblish][dribbblish] theme   |
| **Discord**                | Discord with theme [BetterDiscord][BetterDiscord] |
| **[Eww][eww]**             | Widgets                                           |
| **[neofetch][neofetch]**   | System information tool                           |
| **[onefetch][onefetch]**   | neofetch for git                                  |
| **Google Chrome**          | Browser, [nightTab extension][nightTab]           |
| **Rofi**                   | Application launcher and etc                      |
| **Dunst**                  | Notifications                                     |
| **[picom][picom]**         | Compositor picom fork                             |
| **[flameshot][flameshot]** | Screenshot utility                                |
| **[fzf][fzf]**             | cli fuzzy finder                                  |
| **[bat][bat]**             | cat clone with features                           |
| **[HTTPie][HTTPie]**       | curl alternative / HTTP testing tool              |
| **[tldr][tldr]**           | Simplified man pages                              |
| **[LiteCLI][LiteCLI]**     | SQLite client with Auto-Completion                |

[xmonad]: https://github.com/sebostien/dotfiles/blob/master/.xmonad/README.md
[dribbblish]: https://github.com/morpheusthewhite/spicetify-themes/tree/master/Dribbblish
[BetterDiscord]: https://github.com/BetterDiscord/BetterDiscord
[neofetch]: https://github.com/dylanaraps/neofetch
[onefetch]: https://github.com/o2sh/onefetch
[eww]: https://github.com/elkowar/eww
[nightTab]: ./.config/nightTab.json
[picom]: https://github.com/Arian8j2/picom-jonaburg-fix
[flameshot]: https://github.com/flameshot-org/flameshot/
[fzf]: https://github.com/junegunn/fzf
[HTTPie]: https://httpie.io/cli
[bat]: https://github.com/sharkdp/bat
[tldr]: https://tldr.sh/
[LiteCLI]: https://litecli.com/

## Themes

- [Nordic GTK](https://github.com/EliverLara/Nordic/)
- [Papirus Icon Theme](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme)

## Fonts

- [MesloLGM NF](https://github.com/ryanoasis/nerd-fonts/releases/)
- [Cascadia Code](https://github.com/microsoft/cascadia-code/releases)


## Setup

This is only meant for me and will probably break something.

**Don't proceed**

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