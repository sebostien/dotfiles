module SN.Layouts where

import XMonad ( (|||) )
import XMonad.Hooks.ManageDocks
    ( avoidStruts )
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.Spacing
    ( spacingRaw, Border(Border), Spacing )

import XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall) )
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.Circle (Circle(Circle))

import XMonad.Layout.MultiToggle ( (??), mkToggle, EOT(EOT) )

import XMonad.Layout.MultiToggle.Instances
    ( StdTransformers(NOBORDERS, NBFULL) )

tall = mySpacing gap
     $ ResizableTall 1 (2/50) (1/2) []

grid = mySpacing gap
     $ Grid

circle = mySpacing gap
       $ Circle

gap :: Integer
gap = 8

mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing g = spacingRaw False a True a True
  where
    a = Border g g g g

myLayoutHook = avoidStruts
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ tall ||| grid ||| circle

