# My [XMonad](https://github.com/xmonad/xmonad/) Config

## Developing

Using ghci to typecheck makes developing a lot easier.
Use the following to start it with my personal libraries:

``` console
$ stack ghci xmonad.hs

Prelude> :set -i:lib
Prelude> :r
```

## Dependencies

Debian, Ubuntu

``` console
$ sudo apt install \
> git \
> libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev
```

Fedora

``` console
$ sudo dnf install \
> git \
> libX11-devel libXft-devel libXinerama-devel libXrandr-devel libXScrnSaver-devel
```

Arch

``` console
$ sudo pacman -S \
> git \
> xorg-server xorg-apps xorg-xinit xorg-xmessage \
> libx11 libxft libxinerama libxrandr libxss \
> pkgconf
```

## Installing

This is a rewrite of the offical install Xmonad [guide][xmonadInstall].

First you'll need to install via your systems package manager.
If your distrubtion does not package stack check the [guide][stackInstall] on XMonads github page.

``` console
$ sudo apt install haskell-stack   # Debian, Ubuntu
$ sudo dnf install stack           # Fedora
$ sudo pacman -S stack             # Arch

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

Next we install `xmonad` and `xmonad-contrib`, follow the guide at [GitHub][xmonadInstall].

See the guide to on how to [make XMonad your window manager][xmonadWMGuide]

[stackInstall]: https://docs.haskellstack.org/en/stable/README/
[xmonadInstall]: https://github.com/xmonad/xmonad/blob/master/INSTALL.md
[xmonadWMGuide]: https://github.com/xmonad/xmonad/blob/master/INSTALL.md#make-xmonad-your-window-manager
