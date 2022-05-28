module SN.Globals where

import XMonad (
    KeyMask,
    mod4Mask,
 )

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "google-chrome " -- Sets chrome as browser

myEditor :: String
myEditor = "code --wait " -- Sets VS Code as editor

mySysTray :: String
mySysTray =
    unwords
        [ "killall trayer ; trayer"
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