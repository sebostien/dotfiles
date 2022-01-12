module SN.Globals where

import XMonad
    ( mod4Mask, gets, KeyMask, Dimension, X, XState(windowset) )

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"

mySelectScreenshot :: String
mySelectScreenshot = "select-screenshot"

myScreenshot :: String
myScreenshot = "xfce4-screenshooter"

myBrowser :: String
myBrowser = "google-chrome "  -- Sets chrome as browser

myFileManager :: String
myFileManager = "nautilus "

myEditor :: String
myEditor = "code --wait "  -- Sets VS Code as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#3b4252"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#2196E3"   -- Border color of focused windows

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

mySysTray :: String
mySysTray = unwords [ "trayer"
                    , "--edge top"
                    , "--align right"
                    , "--SetDockType true"
                    , "--SetPartialStrut true"
                    , "--expand true"
                    , "--widthtype pixel"
                    , "--width 260"
                    , "--transparent true"
                    , "--alpha 0"
                    , "--distancefrom right"
                    , "--distance 1100 "
                    , "--tint 0x282c34"
                    , "--monitor 0"
                    , "--padding 0"
                    , "--height 22 &"
                    ]