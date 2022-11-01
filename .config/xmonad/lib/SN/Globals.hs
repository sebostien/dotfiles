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
myBrowser = "firefox "

mySysTray :: String
mySysTray =
    unwords
        [ "killall trayer ; trayer"
        , "--edge top"
        , "--align right"
        , "--SetDockType true"
        , "--expand true"
        , "--transparent true"
        , "--alpha 0"
        , "--distancefrom right"
        , "--distance 480"
        , "--tint 0x242831"
        , "--monitor 1"
        , "--padding 10"
        , "--widthtype pixels"
        , "--width 128"
        , "--height 24"
        ]

killMySysTray :: String
killMySysTray = "killall trayer"
