module SN.Globals where

import XMonad (
  KeyMask,
  mod4Mask,
 )

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "wezterm"

myBrowser :: String
myBrowser = "firefox "
