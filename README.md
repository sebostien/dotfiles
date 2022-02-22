# Dotfiles

config-files for various programs

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

| Name                       | Description                                     |
| -------------------------- | ----------------------------------------------- |
| **[xmonad][xmonad]**       | Window manager                                  |
| **gdm3**                   | Desktop Manager                                 |
| **zsh**                    | zsh shell with oh-my-zsh                        |
| **alacritty**              | Terminal emulator                               |
| **nautilus**               | File manager                                    |
| **Spotify**                | Music, spicetify [Dribbblish][dribbblish] theme |
| **Discord**                | Chat, with [BetterDiscord][BetterDiscord]       |
| **[Eww][eww]**             | Widgets                                         |
| **[neofetch][neofetch]**   | System information tool                         |
| **[onefetch][onefetch]**   | neofetch for git                                |
| **aria2c**                 |                                                 |
| **Google Chrome**          | Browser, [nightTab extension][nightTab]         |
| **[picom][picom]**         | Compositor picom fork                           |
| **[flameshot][flameshot]** | Screenshot utility                              |
| **[TexMaker][texMaker]**   | LaTex editor                                    |


[xmonad]: https://github.com/sebostien/dotfiles/blob/master/.xmonad/README.md
[dribbblish]: https://github.com/morpheusthewhite/spicetify-themes/tree/master/Dribbblish
[BetterDiscord]: https://github.com/BetterDiscord/BetterDiscord
[neofetch]: https://github.com/dylanaraps/neofetch
[onefetch]: https://github.com/o2sh/onefetch
[eww]: https://github.com/elkowar/eww
[nightTab]: ./.config/nightTab.json
[picom]: https://github.com/yshui/picom
[texMaker]: https://www.xm1math.net/texmaker/
[flameshot]: https://github.com/flameshot-org/flameshot/

## Themes

[ayu-dark](https://github.com/ayu-theme/ayu-colors)

## Fonts

- [MesloLGM NF](https://github.com/ryanoasis/nerd-fonts/releases/)
- [Cascadia Code](https://github.com/microsoft/cascadia-code/releases)
