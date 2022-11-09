import Data.Monoid (Endo)
import System.Directory (doesFileExist)

import XMonad

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.StatusBar (withSB)

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Personal libraries
import SN.EwwBar
import SN.Globals
import SN.Keys
import SN.Layouts
import SN.ScratchPad
import SN.Theme

------------------------------------------------------------------------
-- | Startup| ----------------------------------------------------------
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom --experimental-backend"
    spawnOnce "nm-applet"
    spawnOnce "blueman-applet"
    spawnOnce "playerctld"
    spawnOnce "dunst"
    spawnOnce "systemctl --user start pipewire-pulse.service pipewire-pulse.socket"
    -- spawnOnce "systemctl enable --now logid"

    spawnOnce mySysTray
    spawnOnce "nitrogen --restore" -- nitrogen last wallpaper
    spawnOnce myEwwStartupHook

------------------------------------------------------------------------
-- | Window rules | ----------------------------------------------------
------------------------------------------------------------------------
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook =
    manageSpecific
        <+> namedScratchpadManageHook myScratchPads
  where
    manageSpecific =
        composeAll
            [ className =? "confirm" --> doCenterFloat
            , className =? "file_progress" --> doFloat
            , className =? "dialog" --> doFloat
            , className =? "download" --> doFloat
            , className =? "error" --> doFloat
            , className =? "zoom" --> doFloat
            , isRole =? "pop-up" --> doCenterFloat
            , title =? "Bluetooth Devices" --> doCenterFloat
            , isDialog --> doCenterFloat
            , isFullscreen --> doFullFloat
            ]
    isRole = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------------------------
-- | main | -----------------------------------------------
-----------------------------------------------------------

main :: IO ()
main = do
    isDesktop <- doesFileExist "/home/sn/.is_desktop"
    spawn (makeMyKeyFile isDesktop)
    xmonad . withSB myStatusBar . docks $
        ewmh
            def
                { manageHook = myManageHook <+> manageDocks
                , modMask = myModMask
                , terminal = myTerminal
                , startupHook = myStartupHook
                , layoutHook = myLayoutHook
                , borderWidth = myBorderWidth
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , focusFollowsMouse = myFocusFollowsMouse
                }
            `additionalKeysP` (myKeys isDesktop)
