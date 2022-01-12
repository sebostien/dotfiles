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

createLabel :: Color -> String -> String
createLabel c text = unwords
    [ "(label"
    , ":style \"color:" ++ c ++ "\""
    , ":text \"" ++ text ++ "\")"
    ]

yuckPPSep :: Yuck
yuckPPSep = createLabel SNT.base00 "| "

myEwwPP :: PP
myEwwPP = def
    { ppCurrent = createLabel SNT.green . wrap "[" "]"
    , ppExtras  = []
    , ppVisible = createLabel SNT.green
    , ppHidden = createLabel SNT.blue
    , ppHiddenNoWindows = createLabel SNT.magenta
    , ppUrgent = createLabel SNT.orange 
    , ppTitle = createLabel SNT.base2 . shorten 120
    , ppSep = ""
    , ppOrder  = \(ws:l:t:ex) -> [
            createBox "workspaces" "" (concat
                [ ws
                , yuckPPSep
                , createLabel SNT.magenta l
                ])
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
