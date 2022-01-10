module Main where

import SN.Keys (myNamedKeys)

main :: IO ()
main = do let a = unlines $ map show myNamedKeys
          putStr ('\n' : a)
