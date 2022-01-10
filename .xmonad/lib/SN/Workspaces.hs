module SN.Workspaces where

import Data.Maybe ( fromJust )
import qualified Data.Map as M
import XMonad (WorkspaceId)

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaces :: [String]
myWorkspaces = [" dev ", " www ", " dax ", " doc ", " vbox ", " chat ", " mus ", " game ", " gfx "]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

clickable :: String -> String
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices
