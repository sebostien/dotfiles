module SN.Theme where

import XMonad (Dimension)

-----------------------------------------------------------
-- | Theme | ----------------------------------------------
-----------------------------------------------------------

myBorderWidth :: Dimension
myBorderWidth = 1

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = True

background = "#0d1117"
foreground = "#fafafa"
textColor = "#ffffff"

normalBlack = "#0d1117"
normalWhite = "#fafafa"
normalRed = "#F44336"
normalGreen = "#27ae60"
normalYellow = "#Fa951a"
normalBlue = "#2196E3"
normalMagenta = "#9C27B0"
normalCyan = "#00c8d4"

brightBlack = "#485460"
brightWhite = "#ffffff"
brightRed = "#FF5252"
brightGreen = "#2ecc71"
brightYellow = "#FBC02D"
brightBlue = "#03A9F4"
brightMagenta = "#EA80FC"
brightCyan = "#34e7e4"

myNormalBorderColor :: String
myNormalBorderColor = background

myFocusedBorderColor :: String
myFocusedBorderColor = normalGreen
