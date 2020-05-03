module Main where

import Interpreter
import System.Environment

calcStr str = case calc str of
    Left err -> show err
    Right val -> show val

main = do
    args <- getArgs
    text <- readFile (head args)
    putStrLn (calcStr text)
