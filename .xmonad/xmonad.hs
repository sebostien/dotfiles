-- https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
-- https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs

import XMonad
import Data.Monoid ( Endo )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( doCenterFloat, doFullFloat, isDialog, isFullscreen )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.ShowWName ( showWName' )
import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Util.NamedScratchpad ( namedScratchpadFilterOutWorkspacePP, namedScratchpadManageHook )
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.SpawnOnce ( spawnOnce )
import qualified XMonad.Layout.ToggleLayouts as T

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import System.IO ( hPutStrLn )

import SN.Globals
import SN.ScratchPad ( myScratchPads )
import SN.Workspaces ( myWorkspaces, clickable )
import SN.Keys ( myKeys )
import SN.Layouts (tall, myLayoutHook)
import SN.Theme as SNT
import SN.EwwBar
import Graphics.X11 (wM_ICON_NAME)


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

    myEwwStartupHook
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
            , isDialog     --> doCenterFloat
            , isFullscreen -->  doFullFloat
            ]
        isRole = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------------------------
-- | main | -----------------------------------------------
-----------------------------------------------------------
main :: IO ()
main = do
    -- Launching two instances of xmobar on seperate monitors.
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    spawn "~/.config/eww/launch" -- start eww
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = showWName' myShowWNameTheme myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = myEwwLogHook dbus
        } `additionalKeysP` myKeys
