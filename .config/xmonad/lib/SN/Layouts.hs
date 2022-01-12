module SN.Layouts where

import XMonad ( (|||), xmonad, Default(def), XConfig(layoutHook) )
import XMonad.Hooks.ManageDocks
    ( avoidStruts, Direction2D (R, L) )
import XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall) )
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.Renamed ( renamed, Rename(Replace) )
import XMonad.Hooks.EwmhDesktops ( ewmh ) 
import XMonad.Layout.Gaps ( Direction2D(D, U), gaps ) 
import XMonad.Layout.Spacing
    ( spacingRaw, Border(Border), Spacing )
import XMonad.Layout.ShowWName ( showWName' ) 

import XMonad.Layout.MultiToggle ( (??), mkToggle, EOT(EOT) )

import XMonad.Actions.MouseResize ( mouseResize )
import XMonad.Layout.WindowArranger ( windowArrange )

import XMonad.Layout.MultiToggle.Instances
    ( StdTransformers(NOBORDERS, NBFULL) )
import XMonad.Layout.Decoration ( def, ModifiedLayout )

import SN.Theme ( myShowWNameTheme )

-- TODO: GRID, TABS

tall = named "tall"
     $ mySpacing gap
     $ ResizableTall 1 (2/50) (1/2) []

named :: String -> l a -> ModifiedLayout Rename l a
named n = renamed [Replace n]

gap :: Integer
gap = 8

mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing g = spacingRaw False a True a True
  where
    a = Border g g g g

-- addTopBar = noFrillsDeco shrinkText topBarTheme

-- myLayoutHook =  mkToggle (NBFULL ?? NOBORDERS ?? EOT)
--              $ tall ||| tall

myLayoutHook = avoidStruts
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ tall ||| tall

-- Just used to force the type of `myLayoutHook`
forceLayoutType :: IO ()
forceLayoutType = xmonad $ ewmh def { layoutHook = showWName' myShowWNameTheme myLayoutHook }