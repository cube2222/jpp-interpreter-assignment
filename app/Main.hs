module Main where

import Interpreter

calcStr str = case calc str of
    Left err -> show err
    Right val -> show val

main = do
    interact calcStr
    putStrLn ""

{- | Tests
>>> calc "val x = 3; val y = 4; x + y"
"7"
-}