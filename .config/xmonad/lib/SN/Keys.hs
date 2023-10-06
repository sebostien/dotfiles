module SN.Keys (myKeys, myNamedKeys, makeMyKeyFile) where

import SN.EwwBar (myEwwCloseBar, myEwwSpawnBar)
import SN.Globals (killMySysTray, myBrowser, mySysTray, myTerminal)
import System.Exit (exitSuccess)
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
import XMonad.Actions.CycleWS (
  WSType (WSIs),
  nextScreen,
  prevScreen,
 )
import XMonad.Actions.SpawnOn (spawnAndDo)
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts))
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances (
  StdTransformers (NBFULL),
 )
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

-- | Grouped into sections to get a formatted file with keybindings
myNamedKeys :: Bool -> [KeySection]
myNamedKeys isDesktop
  | isDesktop = theKeySections
  | otherwise = theKeySections ++ laptopOnly
 where
  theKeySections =
    [ KeySection
        "XMonad"
        [ ("M-C-r", "Recompile XMonad", spawn "xmonad --recompile")
        , ("M-S-r", "Restart XMonad", spawn "xmonad --restart")
        , ("M-S-q", "Quit XMonad", io exitSuccess)
        , ("M-S-l", "Lock Screen", spawn "betterlockscreen -l dim")
        , ("M-S-k", "Toggle keyboard layout", spawn "~/.config/xmonad/scripts/keyboard.sh")
        , ("M-s k", "Show keybindings", spawn "~/.config/rofi/scripts/keybindings.sh")
        ]
    , KeySection
        "Run"
        [ ("M-S-<Return>", "Run Prompt", spawn "~/.config/rofi/scripts/runmenu.sh")
        , ("M-S-p", "Appsmenu Prompt", spawn "~/.config/rofi/scripts/appsmenu.sh")
        , ("M-<Return>", "Open Terminal", spawn myTerminal)
        , ("M-p t", "Open with unix domain", spawn "wezterm connect unix")
        , ("M-p c", "Open calculator", spawnAndDo (doCenterFloat) "wezterm start -- kalker")
        , ("M-p k", "Open calendar", spawn "wezterm start -- kal")
        , ("M-p b", "Open Browser", spawn myBrowser)
        ]
    , KeySection
        "Window Management"
        [ ("M-S-c", "Kill focused window", kill1)
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
        "Docks"
        [ ("M-S-e o", "Open top bar", spawn myEwwSpawnBar)
        , ("M-S-e c", "Close top bar", spawn myEwwCloseBar)
        , ("M-S-t o", "Open tray", spawn mySysTray)
        , ("M-S-t c", "Close tray", spawn killMySysTray)
        ]
    , KeySection
        "Workspaces"
        [ ("M-.", "Move focus to next monitor", nextScreen)
        , ("M-,", "Move focus to prev monitor", prevScreen)
        , ("M-<Tab>", "Switch to next layout", sendMessage NextLayout)
        ]
    , KeySection
        "Screenshot"
        [ ("<Print>", "Screenshot entire screen", spawn "flameshot screen -c")
        , ("C-<Print>", "Screenshot region", spawn "flameshot gui -d 2000")
        , ("M-S-s", "Screenshot region", spawn "flameshot gui -d 2000")
        ]
    , KeySection
        "Media"
        [ ("<XF86AudioStop>", "Pause audio", spawn "playerctl --all-players pause")
        , ("M-<D>", "Pause audio", spawn "playerctl --all-players pause")
        , ("<XF86AudioPlay>", "Play audio", spawn "playerctl --player=playerctld play")
        , ("M-<U>", "Play audio", spawn "playerctl --player=playerctld play")
        , ("<XF86AudioNext>", "Next audio", spawn "playerctl --player=playerctld next")
        , ("M-<R>", "Next audio", spawn "playerctl --player=playerctld next")
        , ("<XF86AudioPrev>", "Prev audio", spawn "playerctl --player=playerctld previous")
        , ("M-<L>", "Prev audio", spawn "playerctl --player=playerctld previous")
        , ("<XF86AudioMute>", "Mute audio", spawn "amixer -D pulse set Master toggle")
        , ("<XF86AudioRaiseVolume>", "Raise volume", spawn "amixer sset Master 5%+")
        , ("<XF86AudioLowerVolume>", "Lower volume", spawn "amixer sset Master 5%-")
        ]
    ]
  laptopOnly =
    [ KeySection
        "Laptop"
        [ ("<XF86MonBrightnessUp>", "Brightness up", spawn "light -A 5")
        , ("<XF86MonBrightnessDown>", "Brightness down", spawn "light -U 5")
        ]
    ]

myKeys :: Bool -> [(String, X ())]
myKeys isDesktop = map (\(a, _, c) -> (a, c)) ys
  where
    ys = concatMap (\(KeySection _ xs) -> xs) (myNamedKeys isDesktop)

data KeySection = KeySection String [(String, String, X ())]

instance Show KeySection where
  show (KeySection section ks) = "# " ++ section ++ "\n" ++ keys
    where
      keys = unlines $ map (\(a, b, _) -> a ++ replicate (l - length a) ' ' ++ b) ks
      l = maximum (map (\(a, _, _) -> length a) ks) + 3

-- | Create a file with description of all keybindings
makeMyKeyFile :: Bool -> String
makeMyKeyFile isDesktop = "rm -f " ++ fileName ++ " && echo '" ++ myFileKeys ++ "' >> " ++ fileName
 where
  myFileKeys = unlines $ map show (myNamedKeys isDesktop)
  fileName = "~/.config/xmonad/xmonadKeys.txt"
