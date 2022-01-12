# Dotfiles

config-fies for various programs

## Setup

```bash
echo 'ALIAS config="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"' >> $HOME/.zshrc
source ~/.zshrc
echo "dotfiles" >> .gitignore
```

```bash
git clone --bare https://www.github.com/sebostien/dotfiles.git $HOME/dotfiles
config checkout
config config --local status.showUntrackedFiles no
```

## Applications

|               | Description                                     |
| ------------- | ----------------------------------------------- |
| **xmonad**    | Window manager, see [README][xmonad]            |
| **gdm3**      | Desktop Manager                                 |
| **zsh**       | zsh shell with oh-my-zsh                        |
| **alacritty** | Terminal emulator                               |
| **nautilus**  | File manager                                    |
| **Spotify**   | Music, spicetify [Dribbblish][dribbblish] theme |
| **Discord**   | Chat, with [BetterDiscord][BetterDiscord]       |
| **Eww**       | Widgets [Eww](https://github.com/elkowar/eww)   |

[xmonad]: https://github.com/sebostien/dotfiles/blob/master/.xmonad/README.md
[dribbblish]: https://github.com/morpheusthewhite/spicetify-themes/tree/master/Dribbblish
[BetterDiscord]: https://github.com/BetterDiscord/BetterDiscord

**Browser**: Google Chrome
Extensions: - nighttab, config in .config/nightTab.json

### Other

- aria2c
- discord
- spotify
- google-chrome

## Themes

[ayu-dark](https://github.com/ayu-theme/ayu-colors)

## Fonts

- [MesloLGM NF](https://github.com/ryanoasis/nerd-fonts/releases/)
- [Cascadia Code](https://github.com/microsoft/cascadia-code/releases)
