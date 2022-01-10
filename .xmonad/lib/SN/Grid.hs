module SN.Grid where

import XMonad

import XMonad.Actions.GridSelect

import SN.Theme ( myGridColorizer, myFont )

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myGridColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid :: [(String, String)]
myAppGrid = [ ("Firefox"   , "firefox" )
            , ("Chrome"    , "google-chrome" )
            , ("Discord"   , "discord" )
            , ("Spotify"   , "spotify" )
            , ("Steam"     , "steam"   )
            , ("Gimp"      , "gimp"    )
            , ("VirtualBox", "virtualbox")
            , ("Audacity"  , "audacity")
            , ("Blueman"   , "blueman-manager")
            ]