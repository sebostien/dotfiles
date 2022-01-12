{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module SN.EwwBar (myEwwLogHook, myEwwStartupHook, myEwwCloseBar, myEwwSpawnBar) where

import XMonad (X, spawn)
import XMonad.Hooks.DynamicLog

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import SN.Theme as SNT
import SN.Globals (windowCount)
import XMonad.Util.SpawnOnce (spawnOnOnce, spawnOnce)

myEwwStartupHook :: X ()
myEwwStartupHook = spawnOnce "exec ~/Apps/eww/target/release/eww daemon"

myEwwSpawnBar :: X ()
myEwwSpawnBar = spawn "eww open bar"

myEwwCloseBar :: X ()
myEwwCloseBar = spawn "eww close bar"

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
    , ":spacing 5"
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
yuckPPSep = createLabel SNT.base00 "|"

myEwwLogHook :: D.Client -> X ()
myEwwLogHook dbus = dynamicLogWithPP $ def
    { ppOutput = dbusOutput dbus
    , ppCurrent = createLabel SNT.green . wrap "[ " " ]"
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
                , yuckPPSep
                , t
                ])
            ]
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"