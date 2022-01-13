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

type Yuck = String
type EwwClass = String
type Color = String

-- | class
createBox :: EwwClass -> Color -> Yuck -> String
createBox c color yuck = unwords
    [ "(box"
    , ":class \"" ++ c ++ "\""
    , ":halign \"start\""
    , ":valign \"center\""
    , ":spacing 2"
    , ":space-evenly false"
    , yuck ++ ")"
    ]

createLabel :: Color -> String -> String -> String
createLabel c font text = unwords
    [ "(label"
    , ":style \"color:" ++ c ++ "; font-weight: " ++ font ++ "\""
    , ":text \"" ++ text ++ "\")"
    ]

yuckPPSep :: Yuck
yuckPPSep = createLabel SNT.base00 "normal" "| "

myEwwPP :: PP
myEwwPP = def
    { ppCurrent = const (createLabel SNT.green "bold" "\xf111 ")
    , ppExtras  = []
    , ppVisible = const (createLabel "#ffffff" "bold" "\xf111 ")
    , ppHidden = const (createLabel "#acacac" "bold" "\xf10c ")
    , ppHiddenNoWindows = const (createLabel "#6c6c6c" "bold" "\xf10c ")
    , ppUrgent = const (createLabel SNT.orange "bold" "\xf10c ")
    , ppTitle = shorten 120
    , ppSep = ""
    , ppOrder  = \(ws:l:t:ex) -> [
            createBox "workspaces" "" (concat
                [ ws
                -- , yuckPPSep
                -- , createLabel "#ffffff" "normal" l
                ])
            ++ "|||" ++ t -- used by eww to seperate title
            ]
    }

myEwwConfig :: StatusBarConfig
myEwwConfig = def
    { sbLogHook = dynamicLogWithPP myEwwPP
    , sbStartupHook = spawn ""
    , sbCleanupHook = spawn ""
    }


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
