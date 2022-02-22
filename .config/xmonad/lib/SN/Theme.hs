module SN.Theme where

import XMonad ( Window, Default(def), X )
import XMonad.Layout.Tabbed
    ( Theme(decoHeight, urgentTextColor, urgentBorderColor,
            activeTextColor, activeColor, activeBorderColor, inactiveTextColor,
            inactiveColor, inactiveBorderColor, fontName) )
import XMonad.Layout.ShowWName
    ( SWNConfig(swn_color, swn_bgcolor, swn_fade, swn_font) )
import XMonad.Actions.GridSelect ( colorRangeFromClassName )

-----------------------------------------------------------
-- | Theme | ----------------------------------------------                                                   
-----------------------------------------------------------

myFont :: String
myFont = "xft:MesloLGM NF:regular:size=11:antialias=true:hinting=true"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse  = False

myClickJustFocuses :: Bool
myClickJustFocuses   = True

base00,base01,base02,base03,base0,base1,base2,base3 :: String
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#666666"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#fafafa"
base3   = "#fdf6e3"

yellow,orange,red,magenta,violet,blue,cyan,green :: String
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#2196E3"
cyan    = "#2aa198"
green   = "#4CAF50"

myNormalBorderColor :: String
myNormalBorderColor     = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor    = active

active, activeWarn, inactive, focusColor, unfocusColor :: String
active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02


-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = myFont
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }