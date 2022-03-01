module SN.Theme where

import XMonad ( Window, Default(def), X, Dimension )
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

myBorderWidth :: Dimension
myBorderWidth = 2

myFont :: String
myFont = "xft:MesloLGM NF:regular:size=11:antialias=true:hinting=true"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse  = False

myClickJustFocuses :: Bool
myClickJustFocuses   = True

green, orange :: String
green = "#4caf50"
orange = "#cb4b16"

nord0, nord1, nord2, nord3, nord4, nord5, nord6, nord7, nord8, nord9, nord10, nord11, nord12 :: String
nord0 = "#2e3440"

nord1=     "#3b4252"
nord2=     "#434c5e"
nord3=     "#4c566a"

nord4=     "#d8dee9"
nord5=     "#e5e9f0"
nord6=     "#eceff4"

nord7=     "#8fbcbb"
nord8=     "#88c0d0"
nord9=    "#81a1c1"
nord10=    "#5e81ac"
nord11=    "#bf616a"

nord12=    "#d08770"
nord13=    "#ebcb8b"
nord14=    "#a3be8c"
nord15=    "#b48ead"

textColor, accentColor :: String
textColor=         nord4;
accentColor=       nord8;

myNormalBorderColor :: String
myNormalBorderColor     = nord0

myFocusedBorderColor :: String
myFocusedBorderColor    = nord8

active, activeWarn, inactive, focusColor, unfocusColor :: String
active      = nord8
activeWarn  = nord11
inactive    = nord1
focusColor  = nord8
unfocusColor = nord1


-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = myFont
    , swn_fade              = 1.0
    , swn_bgcolor           = nord0
    , swn_color             = "#ffffff"
    }