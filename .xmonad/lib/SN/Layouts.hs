module SN.Layouts where

import XMonad ( (|||), xmonad, Default(def), XConfig(layoutHook) )
import XMonad.Hooks.ManageDocks
    ( Direction2D(R, L, D, U), avoidStruts )
import XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall) )
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.Renamed
    ( Rename(CutWordsRight, AppendWords, PrependWords, CutWordsLeft,
             Replace),
      renamed )
import XMonad.Hooks.EwmhDesktops ( ewmh ) 
import XMonad.Layout.Gaps ( gaps, Gaps )
import XMonad.Layout.Spacing
    ( spacingRaw, Border(Border), Spacing )
import XMonad.Layout.ShowWName ( showWName' ) 

import qualified XMonad.Layout.ToggleLayouts as T
import qualified XMonad.Layout.MultiToggle as MT
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

import XMonad.Layout.MultiToggle ( (??), mkToggle, EOT(EOT) )

import XMonad.Actions.MouseResize ( mouseResize )
import XMonad.Layout.WindowArranger ( windowArrange )
import XMonad.Layout.NoFrillsDecoration
    ( shrinkText, noFrillsDeco )

import Data.Maybe ( fromJust )
import qualified Data.Map as M
import XMonad.Layout.MultiToggle.Instances
    ( StdTransformers(NOBORDERS, NBFULL) )
import XMonad.Layout.Decoration

import SN.Theme ( myShowWNameTheme, topBarTheme )

-- TODO: GRID, TABS

tall = named "tall"
     $ mySpacing (toInteger gap)
     $ addTopBar
     $ ResizableTall 1 (2/50) (1/2) []


named :: String -> l a -> ModifiedLayout Rename l a
named n = renamed [Replace n]

trimNamed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimNamed w n = renamed [CutWordsLeft w, PrependWords n]

suffixed :: String -> l a -> ModifiedLayout Rename l a
suffixed n = renamed [AppendWords n]

trimSuffixed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimSuffixed w n = renamed [CutWordsRight w, AppendWords n]

gap :: Int
gap         = 8

topbar :: Integer
topbar      = 10

sGap :: Int
sGap = quot gap 2

myGaps :: l a -> ModifiedLayout Gaps l a
myGaps = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]

mySmallGaps :: l a -> ModifiedLayout Gaps l a
mySmallGaps = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]

myBigGaps :: l a -> ModifiedLayout Gaps l a
myBigGaps = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing g = spacingRaw False a True a True
  where
    a = Border g g g g

addTopBar = noFrillsDeco shrinkText topBarTheme

myLayoutHook = mouseResize
             $ windowArrange
             $ avoidStruts
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ tall ||| tall

-- Just used to force the type of `myLayoutHook`
forceLayoutType :: IO ()
forceLayoutType = xmonad $ ewmh def { layoutHook = showWName' myShowWNameTheme myLayoutHook }