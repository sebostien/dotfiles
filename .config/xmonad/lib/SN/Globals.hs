module SN.Globals where

import XMonad (
    KeyMask,
    mod4Mask,
 )

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty "

myBrowser :: String
myBrowser = "firefox "

mySysTray :: String
mySysTray = "killall trayer ; stalonetray"

killMySysTray :: String
killMySysTray = "killall stalonetray"
