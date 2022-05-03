module SN.Keys (myKeys, myNamedKeys, makeMyKeyFile) where

import System.Exit (exitSuccess)
import System.IO

import XMonad (
    ChangeLayout (NextLayout),
    Resize (Expand, Shrink),
    X,
    io,
    sendMessage,
    spawn,
    windows,
    withFocused,
 )
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)

import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances (
    StdTransformers (NBFULL),
 )

import XMonad.Actions.CycleWS (
    Direction1D (Next, Prev),
    WSType (WSIs),
    moveTo,
    nextScreen,
    prevScreen,
    shiftTo,
 )
import XMonad.Actions.GridSelect (bringSelected, goToSelected)
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts))
import XMonad.Layout.Gaps (Direction2D (U), GapMessage (ToggleGap))
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

import qualified XMonad.StackSet as W

import SN.Globals (killMySysTray, myBrowser, mySysTray, myTerminal)
import SN.ScratchPad (myScratchPads)

import SN.EwwBar (myEwwCloseBar, myEwwSpawnBar)

myNamedKeys :: [KeySection]
myNamedKeys =
    [ KeySection
        "XMonad"
        [ ("M-C-r", "Recompile XMonad", spawn "xmonad --recompile")
        , ("M-S-r", "Restart XMonad", spawn "xmonad --restart")
        , ("M-S-q", "Quit XMonad", io exitSuccess)
        , ("M-S-k", "Show keybindings", spawn "~/.config/rofi/scripts/keybindings.sh")
        ]
    , KeySection
        "Run"
        [ ("M-S-<Return>", "Run Prompt", spawn "~/.config/rofi/scripts/appsmenu.sh")
        , ("M-<Return>", "Open Terminal", spawn myTerminal)
        , ("M-p s", "Open Spotify", spawn "spotify")
        , ("M-p b", "Open Browser", spawn myBrowser)
        ]
    , KeySection
        "Window Management"
        [ ("M-S-c", "Kill focused window", kill1)
        , ("M-S-a", "Kill all windows in current WS", killAll)
        , ("M-j", "Move focus to next window", windows W.focusDown)
        , ("M-k", "Move focus to prev window", windows W.focusUp)
        , ("M-h", "Shrink window", sendMessage Shrink)
        , ("M-l", "Expand window", sendMessage Expand)
        , ("M-<Space>", "Toggle fullscreen", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-t", "Push floating window to tile", withFocused $ windows . W.sink)
        ]
    , KeySection
        "Notifications"
        [ ("C-<Space>", "Close the last notification", spawn "dunstctl close")
        , ("C-S-<Return>", "Close all notifications", spawn "dunstctl close-all")
        , ("C-S-<Space>", "Pop one notification from history", spawn "dunstctl history-pop")
        ]
    , KeySection
        "Widgets"
        [ ("M-e o", "Open top bar", spawn myEwwSpawnBar)
        , ("M-e c", "Close top bar", spawn myEwwCloseBar)
        , ("M-S-t o", "Open tray", spawn mySysTray)
        , ("M-S-t c", "Close tray", spawn killMySysTray)
        ]
    , KeySection
        "Scratchpad"
        [ ("M-s t", "Toggle terminal scratchpad", namedScratchpadAction myScratchPads "terminal")
        , ("M-s c", "Toggle calculator scratchpad", namedScratchpadAction myScratchPads "calculator")
        , ("M-s b", "Toggle blueman scratchpad", namedScratchpadAction myScratchPads "blueman")
        ]
    , KeySection
        "Workspaces"
        [ ("M-.", "Move focus to next monitor", nextScreen)
        , ("M-,", "Move focus to prev monitor", prevScreen)
        , ("M-S-<KP_Add>", "Shift focused window to next WS", shiftTo Next nonNSP >> moveTo Next nonNSP)
        , ("M-S-<KP_Subtract>", "Shift focused window to prev WS", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        , ("M-<Tab>", "Switch to next layout", sendMessage NextLayout)
        ]
    , KeySection
        "Screenshot"
        [ ("<Print>", "Screenshot entire screen", spawn "flameshot screen -c")
        , ("C-<Print>", "Screenshot region", spawn "flameshot gui -d 2000")
        ]
    , KeySection
        "Media"
        [ ("<XF86AudioStop>", "Pause audio", spawn "/home/sn/.config/xmonad/scripts/pause-music.sh")
        , ("<XF86AudioPlay>", "Play audio", spawn "/home/sn/.config/xmonad/scripts/play-music.sh")
        , ("<XF86AudioNext>", "Next audio", spawn "/home/sn/.config/xmonad/scripts/next-music.sh")
        , ("<XF86AudioPrev>", "Prev audio", spawn "/home/sn/.config/xmonad/scripts/prev-music.sh")
        , ("<XF86AudioMute>", "Mute audio", spawn "amixer -D pulse set Master toggle")
        , ("<XF86AudioRaiseVolume>", "Raise volume", spawn "amixer -D pulse sset Master 2%+")
        , ("<XF86AudioLowerVolume>", "Lower volume", spawn "amixer -D pulse sset Master 2%-")
        ]
    ] -- The following lines are needed for named scratchpads.
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myKeys :: [(String, X ())]
myKeys = map (\(a, _, c) -> (a, c)) ys
  where
    ys = concatMap (\(KeySection _ xs) -> xs) myNamedKeys

data KeySection = KeySection String [(String, String, X ())]

instance Show KeySection where
    show (KeySection section ks) = "# " ++ section ++ "\n" ++ keys
      where
        keys = unlines $ map (\(a, b, _) -> a ++ replicate (l - length a) ' ' ++ b) ks
        l = maximum (map (\(a, _, _) -> length a) ks) + 3

-- | Create a file which has all keybindings
makeMyKeyFile :: String
makeMyKeyFile = "rm -f " ++ fileName ++ " && echo '" ++ myFileKeys ++ "' >> " ++ fileName
  where
    myFileKeys = unlines $ map show myNamedKeys
    fileName = "~/.config/xmonad/xmonadKeys.txt"