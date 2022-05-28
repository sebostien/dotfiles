module SN.ScratchPad where


import XMonad.ManageHook ( (=?), className, title )
import XMonad.Util.NamedScratchpad
    ( NamedScratchpad(NS), customFloating )

import qualified XMonad.StackSet as W

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "calculator" spawnCalc findCalc manageCalc
                , NS "blueman" spawnBlue findBlue manageBlue
                ]
  where
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
    spawnBlue  = "blueman-manager"
    findBlue   = title =? "Bluetooth Devices"
    manageBlue = customFloating $ W.RationalRect l t w h
               where
                 h = 0.4
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w