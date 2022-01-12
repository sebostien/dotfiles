import XMonad
import Data.Monoid ( Endo )
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks (docks, manageDocks)
import XMonad.Hooks.ManageHelpers ( doCenterFloat, doFullFloat, isDialog, isFullscreen )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.ShowWName ( showWName' )
import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Util.NamedScratchpad ( namedScratchpadManageHook )
import XMonad.Util.SpawnOnce ( spawnOnce )

import XMonad.Hooks.StatusBar (statusBarProp, withSB)

-- Personal libraries
import SN.EwwBar
import SN.Globals
import SN.ScratchPad
import SN.Keys
import SN.Layouts
import SN.Workspaces
import SN.Theme (myShowWNameTheme)

------------------------------------------------------------------------
-- | Startup| ----------------------------------------------------------
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawn "killall trayer"  -- kill current trayer on each restart

    spawnOnce "lxsession"
    spawnOnce "picom"
    spawnOnce "nm-applet"
    spawnOnce "blueman-applet"

    spawnOnce myEwwStartupHook
    spawnOnce mySysTray
    spawnOnce "nitrogen --restore"   -- nitrogen last wallpaper
    setWMName "LG3D"


------------------------------------------------------------------------
-- | Window rules | ----------------------------------------------------
------------------------------------------------------------------------
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
        manageSpecific
    <+> namedScratchpadManageHook myScratchPads
    where
        manageSpecific = composeAll
            [ className =? "confirm"            --> doCenterFloat
            , className =? "xmessage"           --> doCenterFloat -- XMonad error
            , className =? "file_progress"      --> doFloat
            , className =? "dialog"             --> doFloat
            , className =? "download"           --> doFloat
            , className =? "error"              --> doFloat
            , className =? "notification"       --> doFloat
            , isRole    =? "pop-up"             --> doCenterFloat
            , className =? "Yad"                --> doCenterFloat
            , className =? "Gimp"               --> doShift " gfx "
            , isRole    =? "browser"            --> doShift " www "
            , className =? "discord"            --> doShift " chat "
            , className =? "Steam"              --> doShift " game "
            , className =? ""                   --> doShift " mus "      -- spotify hopefully
            , title     =? "Bluetooth Devices"  --> doCenterFloat
            , className =? "VirtualBox Manager" --> doShift  " vbox "
            , isDialog     -->  doCenterFloat
            , isFullscreen -->  doFullFloat
            ]
        isRole = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------------------------
-- | main | -----------------------------------------------
-----------------------------------------------------------

mySB = statusBarProp "ewwWidget" (pure myEwwPP)

main :: IO ()
main = do
    spawn makeMyKeyFile
    spawn myEwwSpawnBar
    xmonad . withSB mySB . docks $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = showWName' myShowWNameTheme myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        } `additionalKeysP` myKeys
