module SN.ScratchPad where


import XMonad.ManageHook ( (=?), className, title )
import XMonad.Util.NamedScratchpad
    ( NamedScratchpad(NS), customFloating )

import qualified XMonad.StackSet as W

import SN.Globals ( myTerminal )

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "blueman" spawnBlue findBlue manageBlue
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
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