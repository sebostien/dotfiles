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
base2   = "#eee8d5"
base3   = "#fdf6e3"

yellow,orange,red,magenta,violet,blue,cyan,green :: String
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#98be65"

-- sizes

topbar      = 10

border      = 0
prompt      = 20
status      = 20

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

topBarTheme :: Theme
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }


myGridColorizer :: Window -> Bool -> X (String, String)
myGridColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg


myTabTheme :: Theme
myTabTheme = def { fontName            = myFont
                 , activeColor         = blue
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = blue
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }


-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }