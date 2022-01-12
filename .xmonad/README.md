# XMonad

My config for XMonad

Install [Haskell and cabal](https://www.haskell.org/downloads/)

`cabal install xmonad xmonad-contrib`

https://github.com/xintron/xmonad-log
https://github.com/godbus/dbus

## Dependencies

|              | Description                     |
| ------------ | ------------------------------- |
| xmobar       | statusbar                       |
| trayer       | System tray                     |
| nitrogen     | Background setter               |
| blueman      | Bluetooth manager               |
| nm-applet    | Network manager                 |
| lxsession    | Session manager                 |
| picom        | Xorg compositor                 |
| [rust][rust] | Compilation of Eww              |
| [Eww][eww]   | widgets, bar and notfifications |

[rust]: https://www.rust-lang.org/tools/install
[eww]: https://github.com/elkowar/eww

More specific programs found in [Globals.hs][globals.hs]

[globals.hs]: https://github.com/sebostien/dotfiles/blob/master/.xmonad/lib/SN/Globals.hs

Eww needs to be installed in: `~/Apps/eww/target/release/eww`
