module SN.EwwBar
    ( myEwwPP
    , myEwwStartupHook
    , myEwwCloseBar
    , myEwwSpawnBar
    ) where

import XMonad hiding(Color)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import SN.Theme as SNT
import XMonad.Util.SpawnOnce (spawnOnOnce, spawnOnce)

import qualified XMonad.StackSet as W

myEwwStartupHook :: String
myEwwStartupHook = "exec eww daemon"

myEwwSpawnBar :: String
myEwwSpawnBar = "~/.config/eww/launch_bar.sh"

myEwwCloseBar :: String
myEwwCloseBar = "~/.config/eww/kill_bar.sh"

--  \xf111
--  \xf10c

myEwwPP :: PP
myEwwPP = def
    { ppCurrent = const ("<span foreground='" ++ SNT.green ++ "' font_weight='bold'>\xf111</span>")
    , ppExtras  = []
    , ppVisible = const ("<span foreground='#ffffff' font_weight='bold'>\xf111</span>")
    , ppHidden = const ("<span foreground='#acacac' font_weight='bold'>\xf10c</span>")
    , ppHiddenNoWindows = const ("<span foreground='#6c6c6c' font_weight='bold'>\xf10c</span>")
    , ppUrgent = const ("<span foreground='" ++ SNT.orange ++ "' font_weight='bold'>\xf10c</span>")
    , ppTitle = shorten 120
    , ppSep = ""
    -- used by eww to seperate title
    , ppOrder  = \(ws:l:t:ex) -> [ ws ++ "|||" ++ t ]
    }

myEwwConfig :: StatusBarConfig
myEwwConfig = def
    { sbLogHook = dynamicLogWithPP myEwwPP
    , sbStartupHook = spawn ""
    , sbCleanupHook = spawn ""
    }


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
