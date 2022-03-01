module SN.Globals where

import XMonad
    ( mod4Mask, gets, KeyMask, X, XState(windowset) )

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

mySysTray :: String
mySysTray = unwords [ "killall trayer ; trayer"
                    , "--edge top"
                    , "--align center"
                    , "--SetDockType true"
                    , "--expand true"
                    , "--widthtype percent"
                    , "--width 10"
                    , "--transparent true"
                    , "--alpha 0"
                    , "--distancefrom left"
                    , "--distance 440"
                    , "--tint 0x282c34"
                    , "--monitor 0"
                    , "--padding 0"
                    , "--height 22 &"
                    ]

killMySysTray :: String
killMySysTray = "killall trayer"