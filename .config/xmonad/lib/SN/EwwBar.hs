module SN.EwwBar (
    myEwwStartupHook,
    myEwwCloseBar,
    myEwwSpawnBar,
    myStatusBar,
) where

import XMonad.Hooks.StatusBar (statusBarProp)
import XMonad.Hooks.StatusBar.PP (
    PP,
    def,
    dynamicLogWithPP,
    ppCurrent,
    ppExtras,
    ppHidden,
    ppHiddenNoWindows,
    ppOrder,
    ppSep,
    ppTitle,
    ppUrgent,
    ppVisible,
    shorten,
 )

import SN.Theme as SNT

myEwwStartupHook :: String
myEwwStartupHook = myEwwSpawnBar

myEwwSpawnBar :: String
myEwwSpawnBar = "~/.config/eww/launch_bar.sh"

myEwwCloseBar :: String
myEwwCloseBar = "~/.config/eww/kill_bar.sh"

--  \xf111
--  \xf10c

myEwwPP :: PP
myEwwPP =
    def
        { ppCurrent = const ("<span foreground='" ++ SNT.normalGreen ++ "' font_weight='bold'>\xf111</span>")
        , ppExtras = []
        , ppVisible = const ("<span foreground='#ffffff' font_weight='bold'>\xf111</span>")
        , ppHidden = const ("<span foreground='#acacac' font_weight='bold'>\xf10c</span>")
        , ppHiddenNoWindows = const ("<span foreground='#6c6c6c' font_weight='bold'>\xf10c</span>")
        , ppUrgent = const ("<span foreground='" ++ SNT.normalYellow ++ "' font_weight='bold'>\xf10c</span>")
        , ppTitle = shorten 120
        , ppSep = ""
        , -- used by eww to seperate title
          ppOrder = \(ws : l : t : ex) -> [ws ++ "|||" ++ t]
        }

myStatusBar = statusBarProp "ewwWidget" (pure myEwwPP)