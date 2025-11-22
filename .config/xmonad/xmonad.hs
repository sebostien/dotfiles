-- Personal libraries
import SN.EwwBar
import SN.Globals
import SN.Keys
import SN.Layouts
import SN.Theme

-- Other
import Data.Monoid (Endo)
import System.Directory (doesFileExist)
import Control.Monad (when)

import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.StatusBar (withSB)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

-----------------------------------------------------------------------
-- -| Startup |--------------------------------------------------------
-----------------------------------------------------------------------

myStartupHook :: Bool -> X ()
myStartupHook isLaptop = do
  spawnOnce "nm-applet"                      -- Network manager
  spawnOnce "nitrogen --restore"             -- nitrogen last wallpaper
  spawnOnce "picom"                          -- Start compositor
  spawnOnce "~/scripts/keyboard.sh us"       -- Set keyboard
  spawnOnce "xsetroot -cursor_name left_ptr" -- Default cursor
  spawnOnce "blueman-applet"                 -- Bluetooth manager
  spawnOnce "playerctld"
  spawnOnce "dunst"
  spawnOnce "systemctl --user start pipewire-pulse.service pipewire-pulse.socket"
  spawnOnce myEwwStartupHook
  when isLaptop $ spawnOnce "xinput set-prop 10 339" -- Disable page-up/down
  -- Display settings
  spawnOnce "xset s blank"          -- Show black screen instead of screensaver
  spawnOnce "xset s 600 3600"       -- Black screen after 300s, and cycle image every hour
  spawnOnce "xset dpms 3600 0 3600" -- Turn off monitor after 1h

-----------------------------------------------------------------------
-- -| Window rules |---------------------------------------------------
-----------------------------------------------------------------------
-- -|
-- -| To find the property name associated with a program, use
-- -| > xprop | grep WM_CLASS
-- -| and click on the client you're interested in.
-- -|
myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook = manageSpecific
  where
    manageSpecific =
      composeAll
        [ className =? "confirm"               --> doCenterFloat
        , className =? "file_progress"         --> doFloat
        , className =? "dialog"                --> doFloat
        , className =? "download"              --> doFloat
        , className =? "Gimp"                  --> doFloat
        , className =? "error"                 --> doFloat
        , className =? "zoom"                  --> doFloat
        , className =? "zenity"                --> doCenterFloat
        , isRole    =? "pop-up"                --> doCenterFloat
        , title     =? "Bluetooth Devices"     --> doCenterFloat
        , title     =? "Ulauncher Preferences" --> doCenterFloat
        , isDialog                             --> doCenterFloat
        , isFullscreen                         --> doFullFloat
        ]
    isRole = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------------------------------------
-- -| main |-----------------------------------------------------------
-----------------------------------------------------------------------

main :: IO ()
main = do
  isLaptop <- doesFileExist "/home/sn/.is_laptop"
  spawn (makeMyKeyFile isLaptop)
  xmonad
    . withSB myStatusBar
    . docks
    . ewmhFullscreen
    . ewmh
    $ def
        { manageHook = myManageHook <+> manageDocks
        , modMask = myModMask
        , terminal = myTerminal
        , startupHook = myStartupHook isLaptop
        , layoutHook = myLayoutHook
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , focusFollowsMouse = myFocusFollowsMouse
        }
      `additionalKeysP` myKeys isLaptop
