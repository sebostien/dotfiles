module SN.Keys (myKeys, myNamedKeys, makeMyKeyFile) where

import System.Exit ( exitSuccess )
import System.IO

import XMonad
    ( io,
      spawn,
      sendMessage,
      windows,
      withFocused,
      X,
      ChangeLayout(NextLayout),
      Resize(Expand, Shrink) )
import XMonad.Actions.CopyWindow ( kill1 )
import XMonad.Actions.WithAll ( killAll )

import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
    ( StdTransformers(NBFULL) )

import XMonad.Actions.CycleWS
    ( moveTo,
      nextScreen,
      prevScreen,
      shiftTo,
      WSType(WSIs),
      Direction1D(Prev, Next) )
import XMonad.Actions.GridSelect ( bringSelected, goToSelected )
import XMonad.Util.NamedScratchpad ( namedScratchpadAction )
import XMonad.Hooks.ManageDocks ( ToggleStruts(ToggleStruts) )
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Layout.Gaps ( Direction2D(U), GapMessage(ToggleGap) ) 

import qualified XMonad.StackSet as W

import SN.Globals ( myTerminal, myBrowser )
import SN.ScratchPad ( myScratchPads )

import SN.EwwBar (myEwwSpawnBar, myEwwCloseBar)

myNamedKeys :: [KeySection]
myNamedKeys =
    [ KeySection "XMonad"
        [ ("M-C-r", "Recompile XMonad", spawn "xmonad --recompile")
        , ("M-S-r", "Restart XMonad", spawn "xmonad --restart")
        , ("M-S-q", "Quit XMonad", io exitSuccess)
        , ("M-S-k", "Show keybindings", spawn "/home/sn/.xmonad/bin/showXMonadKeys.sh")
        ]
    , KeySection "Run"
        [ ("M-S-<Return>", "Run Prompt", spawn "dmenu_run -i -p \"Run: \"")
        , ("M-p s", "Open Spotify", spawn "spotify")
        , ("M-p b", "Open Browser", spawn myBrowser)
        , ("M-<Return>", "Open Terminal", spawn myTerminal)
        ]
    , KeySection "Window Management"
        [ ("M-S-c", "Kill focused window", kill1)
        , ("M-S-a", "Kill all windows in current WS", killAll)
        , ("M-j", "Move focus to next window", windows W.focusDown)
        , ("M-k", "Move focus to prev window", windows W.focusUp)
        , ("M-h", "Shrink window", sendMessage Shrink)
        , ("M-l", "Expand window", sendMessage Expand)
        , ("M-<Space>", "Toggle fullscreen", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-t", "Push floating window to tile", withFocused $ windows . W.sink)
        ]
    , KeySection "Eww Widgets"
        [ ("M-e o", "Open top bar", spawn myEwwSpawnBar)
        , ("M-e c", "Close top bar", spawn myEwwCloseBar)
        ]
    , KeySection "Scratchpad"
        [ ("M-s t", "Toggle terminal scratchpad", namedScratchpadAction myScratchPads "terminal")
        , ("M-s c", "Toggle calculator scratchpad", namedScratchpadAction myScratchPads "calculator")
        , ("M-s b", "Toggle blueman scratchpad", namedScratchpadAction myScratchPads "blueman")
        ]
    , KeySection "Workspaces"
        [ ("M-.", "Move focus to next monitor", nextScreen)
        , ("M-,", "Move focus to prev monitor", prevScreen)
        , ("M-S-<KP_Add>", "Shift focused window to next WS", shiftTo Next nonNSP >> moveTo Next nonNSP)
        , ("M-S-<KP_Subtract>", "Shift focused window to prev WS", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        , ("M-<Tab>", "Switch to next layout", sendMessage NextLayout)
        ]
    , KeySection "Media"
        [ ("<XF86AudioStop>", "Pause audio", spawn "/home/sn/.xmonad/bin/pause-music.sh")
        , ("<XF86AudioPlay>", "Play audio", spawn "/home/sn/.xmonad/bin/play-music.sh")
        , ("<XF86AudioNext>", "Next audio", spawn "/home/sn/.xmonad/bin/next-music.sh")
        , ("<XF86AudioPrev>", "Prev audio", spawn "/home/sn/.xmonad/bin/prev-music.sh")
        , ("<XF86AudioMute>", "Mute audio", spawn "amixer -q set Master toggle")
        , ("<XF86AudioRaiseVolume>", "Lower volume", spawn "amixer -q set Master 2%+")
        , ("<XF86AudioLowerVolume>", "Raise volume", spawn "amixer -q set Master 2%-")
        ]
    ] -- The following lines are needed for named scratchpads.
    where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myKeys :: [(String, X ())]
myKeys = map (\(a,_,c) -> (a,c)) ys
    where
        ys = concatMap (\(KeySection _ xs) -> xs) myNamedKeys


data KeySection = KeySection String [(String, String, X ())]

instance Show KeySection where
  show (KeySection section ks) = " # " ++ section ++ "\n" ++ keys
    where
        keys = unlines $ map (\(a,b,_) -> ' ' : a ++ replicate (l - length a) ' ' ++ b) ks
        l = maximum (map (\(a,_,_) -> length a) ks) + 3


-- | Create a file which has all keybindings
makeMyKeyFile :: String
makeMyKeyFile = "echo '" ++ myFileKeys ++ "' >> ~/.config/xmonad/xmonadKeys.txt"
    where
        myFileKeys = unlines $ map show myNamedKeys