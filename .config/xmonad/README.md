# My [XMonad](https://github.com/xmonad/xmonad/) Config

## Developing

Using ghci to typecheck makes developing a lot easier.
Use the following to start it with my personal libraries:

``` console
$ stack ghci xmonad.hs

Prelude> :set -i:lib
Prelude> :r
```

## Installing

This guide is for Fedora, it should be pretty similar on other distros but check
the offical [guide][xmonadInstall]

### Dependencies

``` console
$ sudo dnf install git libX11-devel libXft-devel libXinerama-devel libXrandr-devel libXScrnSaver-devel
```

Install stack and upgrade to latest version

``` console
$ sudo dnf install stack
$ stack upgrade
```

Next you'll need to clone this config into `~/.config/xmonad` and build it with stack.

``` console
$ cd ~/.config/xmonad
$ git clone https://github.com/xmonad/xmonad
$ git clone https://github.com/xmonad/xmonad-contrib
$ stack install
$ stack build
```

Check that `where xmonad` returns `~/.local/bin` so that we use the latest version.
Create symlink to the executable
`sudo ln -s ~/.local/bin/xmonad /usr/bin/xmonad`

See the guide to on how to [make XMonad your window manager][xmonadWMGuide]
depending on your desktop manager.

Probably just: 
``` console
sudo cp ./xmonad.desktop /usr/share/xsessions/
chmod +x ./xmonad-session-rc
```

[xmonadInstall]: https://github.com/xmonad/xmonad/blob/master/INSTALL.md
[xmonadWMGuide]: https://github.com/xmonad/xmonad/blob/master/INSTALL.md#make-xmonad-your-window-manager
